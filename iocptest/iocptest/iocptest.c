

#include <Winsock2.h>
#include <ws2tcpip.h>
#include <mswsock.h>
#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define BUFFER_SIZE 4096
#define MAX_READ_BYTES (BUFFER_SIZE*100 + 100)
#define LISTEN_PORT 2000

static uint8_t data_buffer[BUFFER_SIZE];

int main( int argc, char **argv ) {
	HANDLE iocp_h, p;
	SOCKET sock, conn;
	LPFN_ACCEPTEX acceptexfn = NULL;
	LPFN_GETACCEPTEXSOCKADDRS getaddrsfn = NULL;
	LPFN_CONNECTEX connectexfn = NULL;
    GUID GuidAcceptEx = WSAID_ACCEPTEX;
	GUID GuidGetAddrs = WSAID_GETACCEPTEXSOCKADDRS;
	GUID GuidConnectEx = WSAID_CONNECTEX;
	GUID GuidDisconnectEx = WSAID_DISCONNECTEX;
	DWORD sts, nbytes, flags;
	WSADATA wsadata;
	struct sockaddr_in addr;
	OVERLAPPED overlapped, *overlapped_p;
	BOOL b;
	void *key;
	WSABUF wsabuf;
	int state;

	WSAStartup( MAKEWORD(2,2), &wsadata );

	// create the IOCP
	iocp_h = CreateIoCompletionPort( INVALID_HANDLE_VALUE, NULL, 0, 0 );

	// open the socket, bind and listen
	sock = WSASocketA( AF_INET, SOCK_STREAM, IPPROTO_TCP, NULL, 0, WSA_FLAG_OVERLAPPED );
	if( sock == SOCKET_ERROR ) {
		printf( "Failed to open socket: %d\n", WSAGetLastError() );
	}

	// associate listening socket with icop KEY == 0
	p = CreateIoCompletionPort( (HANDLE)sock, iocp_h, 0, 0 );
	if( !p ) {
		printf( "Failed to open iocp: %d\n", GetLastError() );
	}

	memset( &addr, 0, sizeof(addr) );
	addr.sin_port = htons( LISTEN_PORT );
	addr.sin_addr.S_un.S_addr = INADDR_ANY;
	addr.sin_family = AF_INET;
	sts = bind( sock, (struct sockaddr *)&addr, sizeof(addr) );
	if( sts == SOCKET_ERROR ) {
		printf( "Failed to bind: %d\n", WSAGetLastError() );
	}
	sts = listen( sock, SOMAXCONN );
	if( sts == SOCKET_ERROR ) {
		printf( "Failed to listen: %d\n", WSAGetLastError() );
	}

	// We can't do this:
	// AcceptEx( sock, conn, NULL, 0, 0, 0, NULL, NULL ); 
	// So we have to do this:
	sts = WSAIoctl( sock, SIO_GET_EXTENSION_FUNCTION_POINTER, &GuidAcceptEx, sizeof(GuidAcceptEx), &acceptexfn, sizeof(acceptexfn), &nbytes, NULL, NULL);

	sts = WSAIoctl( sock, SIO_GET_EXTENSION_FUNCTION_POINTER, &GuidGetAddrs, sizeof(GuidGetAddrs), &getaddrsfn, sizeof(getaddrsfn), &nbytes, NULL, NULL);

	sts = WSAIoctl( sock, SIO_GET_EXTENSION_FUNCTION_POINTER, &GuidConnectEx, sizeof(GuidConnectEx), &connectexfn, sizeof(connectexfn), &nbytes, NULL, NULL);

	// we need to allocate the connected socket before passing it to AcceptEx. AcceptEx does the bind() call for us, 
	// then immediately issues a recv call.
	conn = WSASocketA( AF_INET, SOCK_STREAM, IPPROTO_TCP, NULL, 0, WSA_FLAG_OVERLAPPED );
	// associate connected socket with iocp KEY == 1
	p = CreateIoCompletionPort( (HANDLE)conn, iocp_h, 1, 0 );

	memset( &overlapped, 0, sizeof(overlapped) );
	sts = acceptexfn( sock, conn, 
				data_buffer, 0, //sizeof(data_buffer) - (sizeof(struct sockaddr_in) + 16)*2, 
				sizeof(struct sockaddr_in) + 16, sizeof(struct sockaddr_in) + 16, 
				&nbytes, &overlapped );

	do {
		b = GetQueuedCompletionStatus( iocp_h, &nbytes, (ULONG_PTR *)&key, &overlapped_p, INFINITE );
		if( !b ) {
			if( overlapped_p ) {
				printf( "KEY: %d nbytes: %d FAILED code: %d\n", (int)key, nbytes, GetLastError() ); 
			} else {
				printf( "Failed to dequeue: %d\n", GetLastError() );
			}
		} else {
			if( overlapped_p ) {
				printf( "KEY: %d nbytes: %d code: %d\n", (int)key, nbytes, GetLastError() ); 
				if( key == 0 ) {
					// I/O completion on the listening socket. This means we have established a connection
					struct sockaddr_in *local_addr_p, *remote_addr_p;
					int local_addr_len, remote_addr_len;
					getaddrsfn( data_buffer, 0, //sizeof(data_buffer) - (sizeof(struct sockaddr_in) + 16)*2, 
						sizeof(struct sockaddr_in) + 16, sizeof(struct sockaddr_in) + 16,
						(struct sockaddr **)&local_addr_p, &local_addr_len,
						(struct sockaddr **)&remote_addr_p, &remote_addr_len );
					printf( "LOCAL: %d.%d.%d.%d:%d REMOTE:  %d.%d.%d.%d:%d\n", 
						local_addr_p->sin_addr.S_un.S_un_b.s_b1,
						local_addr_p->sin_addr.S_un.S_un_b.s_b2, 
						local_addr_p->sin_addr.S_un.S_un_b.s_b3, 
						local_addr_p->sin_addr.S_un.S_un_b.s_b4,
						ntohs( local_addr_p->sin_port ),
						remote_addr_p->sin_addr.S_un.S_un_b.s_b1,
						remote_addr_p->sin_addr.S_un.S_un_b.s_b2, 
						remote_addr_p->sin_addr.S_un.S_un_b.s_b3, 
						remote_addr_p->sin_addr.S_un.S_un_b.s_b4,
						ntohs( remote_addr_p->sin_port ) );

					// allocate a new unconnected socket and schedule another accept on it
					// new_conn = WSASocket();
					// CreateIoCompletionPort( new_conn, iocp_h, 2, 0 );
					// acceptexfn( sock, new_conn, data_buffer, 0, sizeof(struct sockaddr_in) + 16, sizeof(struct sockaddr_in) + 16, &nbytes, &overlapped );

					// schedule a read on the connection
					flags = 0;
					memset( &overlapped, 0, sizeof(overlapped) );
					wsabuf.buf = (char *)data_buffer;
					wsabuf.len = sizeof(data_buffer);
					sts = WSARecv( conn, &wsabuf, 1, &nbytes, &flags, &overlapped, NULL );		
					if( sts ) {
						sts = GetLastError();
						if( sts != WSA_IO_PENDING ) printf( "Failed to scheule recevive: %d\n", sts ), b = FALSE;
					}
					state = 0;
				} else if( (int)key == 1 ) {
					// I/O completion on the connection
					if( state == 0 ) {
						if( nbytes == 0 ) {
							printf( "Connected closed gracefully\n" );
							b = FALSE;
							break;
						}
						printf( "READ completion, scheduling WRITE\n" );
						wsabuf.buf = (char *)data_buffer;
						wsabuf.len = nbytes;
						memset( &overlapped, 0, sizeof(overlapped) );
						sts = WSASend( conn, &wsabuf, 1, &nbytes, 0, &overlapped, NULL );
						if( sts ) {
							sts = WSAGetLastError();
							if( sts != WSA_IO_PENDING ) printf( "Failed to schedule send: %d\n", sts ), b = FALSE;
						}
						state = 1;
					} else {
						printf( "WRITE completion, scheduling READ\n" );
						wsabuf.buf = (char *)data_buffer;
						wsabuf.len = 4096;
						memset( &overlapped, 0, sizeof(overlapped) );
						flags = 0;
						sts = WSARecv( conn, &wsabuf, 1, &nbytes, &flags, &overlapped, NULL );
						if( sts ) {
							sts = WSAGetLastError();
							if( sts != WSA_IO_PENDING ) printf( "Failed to schedule recv: %d\n", sts ), b = FALSE;
						}
						state = 0;
					}
				}

			} else {
				sts = GetLastError();
				printf( "[%d] Operation failed: %d\n", (int)key, sts );
			}
		}
	} while( b );

	closesocket( sock );
	closesocket( conn );
	CloseHandle( iocp_h );
	WSACleanup();

	return 0;
} 

static int main1( void ) {
	HANDLE iocp_h, device_h, p;
	DWORD nbytes;
	OVERLAPPED overlapped, *overlapped_p;
	BOOL b;
	DWORD sts, offset;
	void *key;
	struct sockaddr_in6 a;



	device_h = CreateFileA( "\\\\.\\PhysicalDrive3", GENERIC_ALL, 0, NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL );
	if( device_h == INVALID_HANDLE_VALUE ) {
		printf( "Failed to open: %d\n", GetLastError() );
	}

	iocp_h = CreateIoCompletionPort( INVALID_HANDLE_VALUE, NULL, 0, 0 );
	if( iocp_h == INVALID_HANDLE_VALUE ) {
		printf( "Failed to open IOCP: %d\n", GetLastError() );
	}

	p = CreateIoCompletionPort( device_h, iocp_h, 0, 0 );
	if( !p ) {
		printf( "Failed to associate device with iocp: %d\n", GetLastError() );
	}

	// start by scheduling a read
	memset( &overlapped, 0, sizeof(overlapped) );
	overlapped.Offset = 0;
	b = ReadFile( device_h, data_buffer, BUFFER_SIZE, &nbytes, &overlapped );
	if( !b ) {
		sts = GetLastError();
		if( sts != ERROR_IO_PENDING ) {
			printf( "Failed to schedule read: %d\n", sts );
		}
	}

	offset = 0;
	do {
		b = GetQueuedCompletionStatus( iocp_h, &nbytes, (ULONG_PTR *)&key, &overlapped_p, INFINITE );
		if( !b ) {
			printf( "Failed to dequeue: %d\n", GetLastError() );
		} else {
			if( overlapped_p ) {
				printf( "Read successfull: %d\n", nbytes );
				// schedule another read
				offset += nbytes;
				memset( &overlapped, 0, sizeof(overlapped) );
				overlapped.Offset = offset;
				b = ReadFile( device_h, data_buffer, BUFFER_SIZE, &nbytes, &overlapped );
				if( !b ) {
					sts = GetLastError();
					if( sts != ERROR_IO_PENDING ) {
						printf( "Failed to schedule read: %d\n", sts );
					}
				}
			} else {
				sts = GetLastError();
				printf( "Read failed: %d\n", sts );
			}
		}
	} while( offset < MAX_READ_BYTES );

	CloseHandle( iocp_h );
	CloseHandle( device_h );
	
	return 0;
}


// -------------------------------
