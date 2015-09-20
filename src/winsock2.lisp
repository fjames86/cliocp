;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines wrappers for the Winsock2 API. 

(in-package #:cliocp)

;; ------------------ startup -------------------------

;; typedef struct WSAData {
;;         WORD                    wVersion;
;;         WORD                    wHighVersion;
;; #ifdef _WIN64
;;         unsigned short          iMaxSockets;
;;         unsigned short          iMaxUdpDg;
;;         char FAR *              lpVendorInfo;
;;         char                    szDescription[WSADESCRIPTION_LEN+1];
;;         char                    szSystemStatus[WSASYS_STATUS_LEN+1];
;; #else
;;         char                    szDescription[WSADESCRIPTION_LEN+1];
;;         char                    szSystemStatus[WSASYS_STATUS_LEN+1];
;;         unsigned short          iMaxSockets;
;;         unsigned short          iMaxUdpDg;
;;         char FAR *              lpVendorInfo;
;; #endif
;; } WSADATA, FAR * LPWSADATA;
(defcstruct wsadata
  (version :uint16)
  (highversion :uint16)
  #+(or x64 x86-64 amd64)(max-sock :uint16)
  #+(or x64 x86-64 amd64)(max-datagram :uint16)
  #+(or x64 x86-64 amd64)(vendor-info :pointer)
  #+(or x64 x86-64 amd64)(desc :char :count 257)
  #+(or x64 x86-64 amd64)(status :char :count 129)
  #-(or x64 x86-64 amd64)(desc :char :count 257)
  #-(or x64 x86-64 amd64)(status :char :count 129)
  #-(or x64 x86-64 amd64)(max-sock :uint16)
  #-(or x64 x86-64 amd64)(max-datagram :uint16)
  #-(or x64 x86-64 amd64)(vendor-info :pointer))
  
  
;; int WSAStartup(
;;   _In_  WORD      wVersionRequested,
;;   _Out_ LPWSADATA lpWSAData
;; );
(defcfun (%wsa-startup "WSAStartup") :int32
  (version :uint16)
  (data :pointer))
(defun wsa-startup ()
  (with-foreign-object (wsadata '(:struct wsadata))
    ;; MAKEWORD(2,2) == #x0202
    (%wsa-startup #x0202 wsadata)))

;; int WSACleanup(void);
(defcfun (wsa-cleanup "WSACleanup") :int)


;; ------------------------- events ---------------------------

;; WSAEVENT WSACreateEvent(void);
(defcfun (wsa-create-event "WSACreateEvent") :pointer)

;; DWORD WINAPI WaitForSingleObject(
;;   _In_ HANDLE hHandle,
;;   _In_ DWORD  dwMilliseconds
;; );
(defcfun (%wait-for-single-object "WaitForSingleObject" :convention :stdcall)
    :pointer
  (handle :pointer)
  (milli :uint32))
(defun wait-for-single-object (handle &optional (timeout 0))
  (%wait-for-single-object handle timeout))

;; DWORD WINAPI WaitForMultipleObjects(
;;   _In_       DWORD  nCount,
;;   _In_ const HANDLE *lpHandles,
;;   _In_       BOOL   bWaitAll,
;;   _In_       DWORD  dwMilliseconds
;; );
(defcfun (%wait-for-multiple-objects "WaitForMultipleObjects" :convention :stdcall)
    :uint32
  (ncount :uint32)
  (handles :pointer)
  (wait-all :boolean)
  (milli :uint32))

(defun wait-for-multiple-objects (handles &optional (timeout 0))
  (with-foreign-object (hbuffer :pointer (length handles))
    (do ((h handles (cdr h))
	 (i 0 (1+ i)))
	((null h))
      (setf (mem-aref hbuffer i) (car h)))
    (%wait-for-multiple-objects (length handles)
				hbuffer 
				t 
				timeout)))

;; BOOL WINAPI CloseHandle(
;;   _In_ HANDLE hObject
;; );
(defcfun (close-handle "CloseHandle" :convention :stdcall) :boolean
  (handle :pointer))





;; ------------------------ sockets -----------------------------

(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 23)
(defconstant +sock-stream+ 1)
(defconstant +sock-datagram+ 2)
(defconstant +wsa-flag-overlapped+ 1)

;; SOCKET WSASocket(
;;   _In_ int                af,
;;   _In_ int                type,
;;   _In_ int                protocol,
;;   _In_ LPWSAPROTOCOL_INFO lpProtocolInfo,
;;   _In_ GROUP              g,
;;   _In_ DWORD              dwFlags
;; );
(defcfun (%wsa-socket "WSASocketA") :pointer 
  (af :int32)
  (type :int32)
  (prot :int32)
  (info :pointer)
  (group :uint32)
  (flags :uint32))

(defun open-socket (&key (family :ipv4) (type :stream))
  (%wsa-socket (ecase family
		 (:ipv4 +af-inet+) 
		 (:ipv6 +af-inet6+))
	       (ecase type
		 (:stream +sock-stream+) 
		 (:datagram +sock-datagram+))
	       0 ;; choose default (TCP for stream, UDP for datagram)
	       (null-pointer)
	       0
	       +wsa-flag-overlapped+))

;; struct sockaddr_in {
;;         short   sin_family;
;;         u_short sin_port;
;;         struct  in_addr sin_addr;
;;         char    sin_zero[8];
;; };
(defcstruct sockaddr-in
  (family :int16)
  (port :uint16)
  (addr :uint8 :count 4)
  (zero :uint8 :count 8))
(defun init-sockaddr-in (ptr addr port)
  (memset ptr '(:struct sockaddr-in))
  (setf (foreign-slot-value ptr '(:struct sockaddr-in) 'family)
	+af-inet+
	(foreign-slot-value ptr '(:struct sockaddr-in) 'port)
	(htons port))
  (dotimes (i 4)
    (setf (mem-aref (foreign-slot-value ptr '(:struct sockaddr-in) 'addr)
		    :uint8
		    i)
	  (aref addr i)))
  ptr)
(defun parse-sockaddr-in (ptr)
  (let ((family (foreign-slot-value ptr '(:struct sockaddr-in) 'family))
	(p (inc-pointer ptr (foreign-slot-offset '(:struct sockaddr-in) 'addr)))
	(addr (make-array 4)))
    (unless (= family +af-inet+)
      (error "Socket family ~A, expected AF_INET(~A)" family +af-inet+))
    (dotimes (i 4)
      (setf (aref addr i) (mem-aref p :uint8 i)))
    (values addr 
	    (ntohs (foreign-slot-value ptr '(:struct sockaddr-in) 'port)))))

;; typedef struct sockaddr_in6 {
;;     ADDRESS_FAMILY sin6_family; // AF_INET6.
;;     USHORT sin6_port;           // Transport level port number.
;;     ULONG  sin6_flowinfo;       // IPv6 flow information.
;;     IN6_ADDR sin6_addr;         // IPv6 address.
;;     union {
;;         ULONG sin6_scope_id;     // Set of interfaces for a scope.
;;         SCOPE_ID sin6_scope_struct; 
;;     };
;; }
(defcstruct sockaddr-in6 
  (family :uint16)
  (port :uint16)
  (flowinfo :uint32)
  (addr :uint8 :count 16)
  (scope :uint32))
(defun init-sockaddr-in6 (ptr addr port)
  (memset ptr '(:struct sockaddr-in6))
  (setf (foreign-slot-value ptr '(:struct sockaddr-in6) 'family) +af-inet6+
	(foreign-slot-value ptr '(:struct sockaddr-in6) 'port) (htons port))
  (let ((p (inc-pointer ptr (foreign-slot-offset '(:struct sockaddr-in6) 'addr))))
    (dotimes (i 16)
      (setf (mem-aref p :uint8 i) (aref addr i))))
  ptr)
(defun parse-sockaddr-in6 (ptr)
  (let ((family (foreign-slot-value ptr '(:struct sockaddr-in6) 'family))
	(p (inc-pointer ptr (foreign-slot-offset '(:struct sockaddr-in6) 'addr)))
	(addr (nibbles:make-octet-vector 16)))
    (unless (= family +af-inet6+) 
      (error "Socket family ~A, expected AF_INET6(~A)" family +af-inet6+))
    (dotimes (i 16)
      (setf (aref addr i) (mem-aref p :uint8 i)))
    (values addr
	    (ntohs (foreign-slot-value ptr '(:struct sockaddr-in6) 'port)))))
    
;; -----------------------------------------------------------
  

;; int bind(
;;   _In_ SOCKET                s,
;;   _In_ const struct sockaddr *name,
;;   _In_ int                   namelen
;; );
(defcfun (%bind "bind") :int32
  (sock :pointer)
  (addr :pointer)
  (len :int32))

;; u_short WSAAPI htons(
;;   _In_ u_short hostshort
;; );
(defcfun (htons "htons" :convention :stdcall) :uint16
  (hs :uint16))

;; u_short WSAAPI ntohs(
;;   _In_ u_short netshort
;; );
(defcfun (ntohs "ntohs" :convention :stdcall) :uint16
  (n :uint16))

;; FIXME: support ipv6 addresses
(defun socket-bind (sock &optional (port 0) address)
  (with-foreign-object (addr '(:struct sockaddr-in))
    (init-sockaddr-in addr (or address #(0 0 0 0)) port)
    (let ((res (%bind sock
		      addr 
		      (foreign-type-size '(:struct sockaddr-in)))))
      (if (= res -1)
	  (maybe-get-last-error)
	  nil))))

;; int listen(
;;   _In_ SOCKET s,
;;   _In_ int    backlog
;; );
(defcfun (%listen "listen") :int32
  (sock :pointer)
  (backlog :int32))

(defun socket-listen (sock &optional backlog)
  (let ((res (%listen sock (or backlog #x7fffffff)))) ;; SOMAXCONN
    (if (= res -1)
	(maybe-get-last-error)
	nil)))


;; ----------------- events ------------------
;; We might not need these, but could prove useful.

;; int WSAEventSelect(
;;   _In_ SOCKET   s,
;;   _In_ WSAEVENT hEventObject,
;;   _In_ long     lNetworkEvents
;; );
(defcfun (%wsa-event-select "WSAEventSelect") :int32
  (sock :pointer)
  (event :pointer)
  (events :uint32))

(defun wsa-event-select (sock event &rest events)
  (let ((e (reduce (lambda (sum ev)
		     (logior sum (ecase ev
				   (:read 1)
				   (:write 2)
				   (:oob 4)
				   (:accept 8)
				   (:connect 16)
				   (:close 32))))
		   events)))
    (%wsa-event-select sock event e)))

;; typedef struct _WSANETWORKEVENTS {
;;   long lNetworkEvents;
;;   int  iErrorCode[FD_MAX_EVENTS];
;; } WSANETWORKEVENTS, *LPWSANETWORKEVENTS;
(defcstruct wsa-network-events 
  (events :uint32)
  (ierrors :uint32 :count 10))

;; int WSAEnumNetworkEvents(
;;   _In_  SOCKET             s,
;;   _In_  WSAEVENT           hEventObject,
;;   _Out_ LPWSANETWORKEVENTS lpNetworkEvents
;; );
(defcfun (%wsa-enum-network-events "WSAEnumNetworkEvents") :int32
  (sock :pointer)
  (event :pointer)
  (events :pointer))

(defun wsa-enum-network-events (sock event)
  (with-foreign-object (events '(:struct wsa-network-events))
    (%wsa-enum-network-events sock event events)
    (do ((ret nil)
	 (i 0 (1+ i))
	 (ev (foreign-slot-value events '(:struct wsa-network-events) 'events)))
	((= i 6) (nreverse ret))
      (unless (zerop (logand ev (ash 1 i)))
	(push (list (ecase i
		      (0 :read)
		      (1 :write)
		      (2 :oob)
		      (3 :accept)
		      (4 :connect)
		      (5 :close))
		    (mem-aref (foreign-slot-value events '(:struct wsa-network-events) 'ierrors)
			      :uint32
			      i))
	      ret)))))

;; ---------------------------------------------------
		      

;; SOCKET WSAAccept(
;;   _In_    SOCKET          s,
;;   _Out_   struct sockaddr *addr,
;;   _Inout_ LPINT           addrlen,
;;   _In_    LPCONDITIONPROC lpfnCondition,
;;   _In_    DWORD_PTR       dwCallbackData
;; );
(defcfun (%wsa-accept "WSAAccept") :pointer 
  (sock :pointer)
  (addr :pointer)
  (len :pointer)
  (cb :pointer)
  (data :pointer))

(defun wsa-accept (sock)
  "Blocking call to accept a connection.
SOCK ::= listening socket

Returns (values conn remote-address remote-port).
"
  (with-foreign-objects ((addr '(:struct sockaddr-in))
			 (len :uint32))
    (let ((conn (%wsa-accept sock
			     addr 
			     len
			     (null-pointer)
			     (null-pointer))))
      (multiple-value-bind (a p) (parse-sockaddr-in addr)
	(values conn a p)))))

;; int closesocket(
;;   _In_ SOCKET s
;; );
(defcfun (close-socket "closesocket") :int32
  (sock :pointer))

;; int shutdown(
;;   _In_ SOCKET s,
;;   _In_ int    how
;; );
(defcfun (%shutdown-socket "shutdown") :int32
  (sock :pointer)
  (how :int32))
(defun socket-shutdown (sock &optional (how :send))
  (let ((res (%shutdown-socket sock 
			       (ecase how
				 (:send 1)
				 (:both 2)
				 (:receive 0)))))
    (if (zerop res)
	nil
	(maybe-get-last-error))))

;; typedef struct __WSABUF {
;;   u_long   len;
;;   char FAR *buf;
;; } WSABUF, *LPWSABUF;
(defcstruct wsabuf 
  (len :uint32)
  (buffer :pointer))
(defun init-wsabuf (wsabuf buffer count)
  (setf (foreign-slot-value wsabuf '(:struct wsabuf) 'len) count
	(foreign-slot-value wsabuf '(:struct wsabuf) 'buffer) buffer)
  wsabuf)

;; int WSARecv(
;;   _In_    SOCKET                             s,
;;   _Inout_ LPWSABUF                           lpBuffers,
;;   _In_    DWORD                              dwBufferCount,
;;   _Out_   LPDWORD                            lpNumberOfBytesRecvd,
;;   _Inout_ LPDWORD                            lpFlags,
;;   _In_    LPWSAOVERLAPPED                    lpOverlapped,
;;   _In_    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
;; );
(defcfun (%wsa-recv "WSARecv") :int32
  (sock :pointer)
  (wsabuf :pointer)
  (bcount :uint32)
  (nbytes :pointer)
  (flags :pointer)
  (overlapped :pointer)
  (cb :pointer))

(defun wsa-recv (conn wsabuf-addr overlapped)
  (with-foreign-objects ((nbytes :uint32)
			 (flags :uint32))
    (setf (mem-aref flags :uint32 0) 0)
    (let ((res (%wsa-recv conn
                          wsabuf-addr
                          1
                          nbytes
                          flags
                          overlapped 
                          (null-pointer))))
      (if (zerop res)
          nil
          (maybe-get-last-error)))))


;; int WSARecvFrom(
;;   _In_    SOCKET                             s,
;;   _Inout_ LPWSABUF                           lpBuffers,
;;   _In_    DWORD                              dwBufferCount,
;;   _Out_   LPDWORD                            lpNumberOfBytesRecvd,
;;   _Inout_ LPDWORD                            lpFlags,
;;   _Out_   struct sockaddr                    *lpFrom,
;;   _Inout_ LPINT                              lpFromlen,
;;   _In_    LPWSAOVERLAPPED                    lpOverlapped,
;;   _In_    LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
;; );
(defcfun (%wsa-recv-from "WSARecvFrom") :int32
  (socket :pointer)
  (buffers :pointer)
  (count :uint32)
  (nbytes :pointer)
  (flags :pointer)
  (addr :pointer)
  (len :pointer)
  (overlapped :pointer)
  (cb :pointer))

(defun wsa-recv-from (socket wsabuf overlapped &optional addr-buffer addr-len-ptr)
  (with-foreign-objects ((nbytes :uint32)
			 (flags :uint32))
    (when addr-len-ptr
      (setf (mem-aref addr-len-ptr :uint32) (foreign-type-size '(:struct sockaddr-in))))
    (setf (mem-aref flags :uint32) 0)
    (let ((res (%wsa-recv-from socket
			       wsabuf
			       1
			       nbytes
			       flags
			       (or addr-buffer (null-pointer))
			       (or addr-len-ptr (null-pointer))
			       overlapped
			       (null-pointer))))
      (if (zerop res)
	  nil
	  (maybe-get-last-error)))))
			
;; int WSASend(
;;   _In_  SOCKET                             s,
;;   _In_  LPWSABUF                           lpBuffers,
;;   _In_  DWORD                              dwBufferCount,
;;   _Out_ LPDWORD                            lpNumberOfBytesSent,
;;   _In_  DWORD                              dwFlags,
;;   _In_  LPWSAOVERLAPPED                    lpOverlapped,
;;   _In_  LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
;; );
(defcfun (%wsa-send "WSASend") :int32
  (sock :pointer)
  (wsabuf :pointer)
  (bcount :uint32)
  (nbytes :pointer)
  (flags :uint32)
  (overlapped :pointer)
  (cb :pointer))

(defun wsa-send (conn wsabuf-addr overlapped)
  (with-foreign-object (nbytes :uint32)
    (let ((res (%wsa-send conn
                          wsabuf-addr
                          1
                          nbytes
                          0
                          overlapped 
                          (null-pointer))))
      (if (zerop res)
          nil
          (maybe-get-last-error)))))


;; int WSASendTo(
;;   _In_  SOCKET                             s,
;;   _In_  LPWSABUF                           lpBuffers,
;;   _In_  DWORD                              dwBufferCount,
;;   _Out_ LPDWORD                            lpNumberOfBytesSent,
;;   _In_  DWORD                              dwFlags,
;;   _In_  const struct sockaddr              *lpTo,
;;   _In_  int                                iToLen,
;;   _In_  LPWSAOVERLAPPED                    lpOverlapped,
;;   _In_  LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
;; );
(defcfun (%wsa-send-to "WSASendTo") :int32
  (socket :pointer)
  (wsabuf :pointer)
  (count :uint32)
  (nbytes :uint32)
  (flags :uint32)
  (to :pointer)
  (len :uint32)
  (overlapped :pointer)
  (cb :pointer))

(defun wsa-send-to (socket wsabuf overlapped address port)
  (with-foreign-objects ((nbytes :uint32)
			 (addr '(:struct sockaddr-in)))
    (init-sockaddr-in addr address port)
    (let ((res (%wsa-send-to socket
			     wsabuf
			     1
			     nbytes
			     0
			     addr
			     (foreign-type-size '(:struct sockaddr-in))
			     overlapped
			     (null-pointer))))
      (if (zerop res)
	  nil
	  (maybe-get-last-error)))))
  


;; int setsockopt(
;;   _In_       SOCKET s,
;;   _In_       int    level,
;;   _In_       int    optname,
;;   _In_ const char   *optval,
;;   _In_       int    optlen
;; );
(defcfun (%set-sock-opt "setsockopt") :int32
  (sock :pointer)
  (level :int32)
  (name :int32)
  (val :pointer)
  (len :int32))
(defgeneric set-socket-option (sock level name &rest args))
;; SOL_SOCKET options
(defmethod set-socket-option (sock (level (eql :socket)) (name (eql :update-connect-context)) &rest args)
  (declare (ignore args))
  (with-foreign-object (b :uint32)
    (setf (mem-aref b :uint32) 1)
    (let ((res (%set-sock-opt sock 
			      #xffff ;; SOL_SOCKET
			      #x7010 ;; SO_UPDATE_CONNECT_CONTEXT
			      b
			      (foreign-type-size :uint32))))
      (if (zerop res)
	  nil
	  (maybe-get-last-error)))))


		 
;; ------------------- Extra functions --------------
;; We want to use the special asynchronous versions of connect/accept.
;; However, these functions are not exported from ws2_32.dll. In order
;; to use them we have to go through this ceremony of calling WSAIoctl().
;; 

;; int WSAIoctl(
;;   _In_  SOCKET                             s,
;;   _In_  DWORD                              dwIoControlCode,
;;   _In_  LPVOID                             lpvInBuffer,
;;   _In_  DWORD                              cbInBuffer,
;;   _Out_ LPVOID                             lpvOutBuffer,
;;   _In_  DWORD                              cbOutBuffer,
;;   _Out_ LPDWORD                            lpcbBytesReturned,
;;   _In_  LPWSAOVERLAPPED                    lpOverlapped,
;;   _In_  LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
;; );
(defcfun (%wsa-ioctl "WSAIoctl") :int32
  (sock :pointer)
  (code :uint32)
  (in-buffer :pointer)
  (in-count :uint32)
  (out-buffer :pointer)
  (out-count :uint32)
  (nbytes :pointer)
  (overlapped :pointer)
  (cb :pointer))

(defun wsa-ioctl (sock code in-buff in-count out-buff out-count)
  (with-foreign-object (nbytes :uint32)
    (let ((res (%wsa-ioctl sock
                           code
                           in-buff
                           in-count 
                           out-buff
                           out-count 
                           nbytes
                           (null-pointer)
                           (null-pointer))))
      (if (zerop res)
          nil
          (pounds::get-last-error)))))

;; typedef struct _GUID {
;;     unsigned long  Data1;
;;     unsigned short Data2;
;;     unsigned short Data3;
;;     unsigned char  Data4[ 8 ];
;; } GUID;
(defcstruct guid 
  (data1 :uint32)
  (data2 :uint16)
  (data3 :uint16)
  (data4 :uint8 :count 8))
(defun init-guid (ptr data1 data2 data3 data4)
  (setf (foreign-slot-value ptr '(:struct guid) 'data1)
        data1
        (foreign-slot-value ptr '(:struct guid) 'data2)
        data2
        (foreign-slot-value ptr '(:struct guid) 'data3)
        data3)
  (do ((i 0 (1+ i))
       (d data4 (cdr d))
       (p (inc-pointer ptr (foreign-slot-offset '(:struct guid) 'data4))))
      ((null d))
    (setf (mem-aref p :uint8 i) (car d)))
  ptr)
        
(defconstant +SIO-GET-EXTENSION-FUNCTION-POINTER+ #xc8000006)

;; We'd end up with 4 copies of this so instead lets write it as a macro
;; (defparameter *accept-ex-guid* '(#xB5367DF1 #xCBAC #x11CF (#x95 #xCA #x00 #x80 #x5F #x48 #xA1 #x92)))
;; (defvar *accept-ex* nil)
;; (defun get-accept-ex (sock)
;;   (with-foreign-objects ((guid '(:struct guid))
;;                          (ptr :pointer))
;;     (apply #'init-guid guid *accept-ex-guid*)
;;     (wsa-ioctl sock 
;;                +SIO-GET-EXTENSION-FUNCTION-POINTER+
;;                guid
;;                (foreign-type-size '(:struct guid))
;;                ptr 
;;                (foreign-type-size :pointer))
;;     (setf *accept-ex* (mem-aref ptr :pointer 0))
;;     *accept-ex*))

(macrolet ((def-get-ex-fn (name guid)
	     `(progn
		(defvar ,(alexandria:symbolicate '* name '*) nil)
		(defun ,(alexandria:symbolicate 'get- name) (sock)
		  (with-foreign-objects ((guid '(:struct guid))
					 (ptr :pointer))
		    (apply #'init-guid guid ',guid)
		    (wsa-ioctl sock 
			       +SIO-GET-EXTENSION-FUNCTION-POINTER+
			       guid
			       (foreign-type-size '(:struct guid))
			       ptr 
			       (foreign-type-size :pointer))
		    (setf ,(alexandria:symbolicate '* name '*) (mem-aref ptr :pointer 0))
		    ,(alexandria:symbolicate '* name '*))))))
		  
  ;; #define WSAID_ACCEPTEX {0xb5367df1,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
  (def-get-ex-fn accept-ex (#xB5367DF1 #xCBAC #x11CF (#x95 #xCA #x00 #x80 #x5F #x48 #xA1 #x92)))
  ;; #define WSAID_GETACCEPTEXSOCKADDRS {0xb5367df2,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
  (def-get-ex-fn get-addrs-ex (#xB5367DF2 #xCBAC #x11CF (#x95 #xCA #x00 #x80 #x5F #x48 #xA1 #x92)))
  ;; #define WSAID_CONNECTEX {0x25a207b9,0xddf3,0x4660,{0x8e,0xe9,0x76,0xe5,0x8c,0x74,0x06,0x3e}}
  (def-get-ex-fn connect-ex (#x25A207B9 #xDDF3 #x4660 (#x8E #xE9 #x76 #xE5 #x8C #x74 #x06 #x3E)))
  ;; #define WSAID_DISCONNECTEX {0x7fda2e11,0x8630,0x436f,{0xa0, 0x31, 0xf5, 0x36, 0xa6, 0xee, 0xc1, 0x57}}
  (def-get-ex-fn disconnect-ex (#x7FDA2E11 #x8630 #x436F (#xA0 #x31 #xF5 #x36 #xA6 #xEE #xC1 #x57))))

;; int WSAGetLastError(void);
(defcfun (%wsa-get-last-error "WSAGetLastError") :int32)
(defun maybe-get-last-error ()
  (let ((code (%wsa-get-last-error)))
    (case code 
      ((0 997) nil) ;; WSA_IO_PENDING == 997
      (otherwise (error 'pounds::win-error :code code)))))

;; BOOL AcceptEx(
;;   _In_  SOCKET       sListenSocket,
;;   _In_  SOCKET       sAcceptSocket,
;;   _In_  PVOID        lpOutputBuffer,
;;   _In_  DWORD        dwReceiveDataLength,
;;   _In_  DWORD        dwLocalAddressLength,
;;   _In_  DWORD        dwRemoteAddressLength,
;;   _Out_ LPDWORD      lpdwBytesReceived,
;;   _In_  LPOVERLAPPED lpOverlapped
;; );
(defun accept-ex (sock conn buffer overlapped)
  "Schedule to accept a connection on the local listening socket SOCK and unconnected socket CONN.

SOCK ::= listening socket
CONN ::= unconnected socket, as returned from open-socket
BUFER ::= pointer to data buffer to receive result
OVERLAPPED ::= pointer to overlapped structure.
"
  (with-foreign-object (nbytes :uint32)
    (let ((b (foreign-funcall-pointer *accept-ex* ()
                                        :pointer sock 
                                        :pointer conn
                                        :pointer buffer  
                                        :uint32 0
                                        :uint32 (+ (foreign-type-size '(:struct sockaddr-in)) 16)
                                        :uint32 (+ (foreign-type-size '(:struct sockaddr-in)) 16)
                                        :pointer nbytes
                                        :pointer overlapped
                                        :boolean)))
      (if b
          nil
          (maybe-get-last-error)))))
            

;; void GetAcceptExSockaddrs(
;;   _In_  PVOID      lpOutputBuffer,
;;   _In_  DWORD      dwReceiveDataLength,
;;   _In_  DWORD      dwLocalAddressLength,
;;   _In_  DWORD      dwRemoteAddressLength,
;;   _Out_ LPSOCKADDR *LocalSockaddr,
;;   _Out_ LPINT      LocalSockaddrLength,
;;   _Out_ LPSOCKADDR *RemoteSockaddr,
;;   _Out_ LPINT      RemoteSockaddrLength
;; );
(defun get-addrs-ex (buffer)
  "Get the accepted connection local and remote addresses. 
BUFFER ::= pointer to buffer containing the result data from a previous call to AcceptEx.
Must be at least 32 bytes long.

Returns (values remote-address remote-port local-address local-port)."
  (with-foreign-objects ((local-addr :pointer)
                         (remote-addr :pointer)
                         (local-len :uint32)
                         (remote-len :uint32))
    (foreign-funcall-pointer *get-addrs-ex* ()
                             :pointer buffer 
                             :uint32 0
                             :uint32 (+ (foreign-type-size '(:struct sockaddr-in)) 16)
                             :uint32 (+ (foreign-type-size '(:struct sockaddr-in)) 16)
                             :pointer local-addr 
                             :pointer local-len
                             :pointer remote-addr 
                             :pointer remote-len)
    ;; convert the addresses into octet vectors 
    (values-list (append (let ((p (mem-aref remote-addr :pointer 0)))
			   (multiple-value-list (parse-sockaddr-in p)))
			 (let ((p (mem-aref local-addr :pointer 0)))
			   (multiple-value-list (parse-sockaddr-in p)))))))


;; BOOL PASCAL ConnectEx(
;;   _In_     SOCKET                s,
;;   _In_     const struct sockaddr *name,
;;   _In_     int                   namelen,
;;   _In_opt_ PVOID                 lpSendBuffer,
;;   _In_     DWORD                 dwSendDataLength,
;;   _Out_    LPDWORD               lpdwBytesSent,
;;   _In_     LPOVERLAPPED          lpOverlapped
;; );
(defun connect-ex (sock addr port overlapped)
  "Schedule a connection on the socket to the address.

SOCK ::= unconnected socket
ADDR ::= octet vector of IP address 
PORT ::= port 
OVERLAPPED ::= pointer to overlapped structure.
"
  (with-foreign-objects ((nbytes :uint32)
			 (saddr '(:struct sockaddr-in)))
    (init-sockaddr-in saddr addr port)
    (let ((res (foreign-funcall-pointer *connect-ex* (:convention :stdcall)
					:pointer sock
					:pointer saddr 
					:int32 (foreign-type-size '(:struct sockaddr-in))
					:pointer (null-pointer)
					:uint32 0
					:pointer nbytes
					:pointer overlapped
					:boolean)))
      (if res 
	  nil
	  (maybe-get-last-error)))))

;; BOOL DisconnectEx(
;;   _In_ SOCKET       hSocket,
;;   _In_ LPOVERLAPPED lpOverlapped,
;;   _In_ DWORD        dwFlags,
;;   _In_ DWORD        reserved
;; );                           
(defun disconnect-ex (sock overlapped &optional (reuse t))
  "Disconnect the connected sock and allow it to be reused in subsequent calls to AcceptEx or ConnectEx."
  (foreign-funcall-pointer *disconnect-ex* ()
                           :pointer sock
                           :pointer overlapped
                           :uint32 (if reuse 2 0) ;; TF_REUSE_SOCKET == 2
                           :uint32 0))
             
;; ------------------- initializtion ------------------

;; we need to initialzie by getting the Ex functions, but only do this once. 
;; We therefore do it once, at load-time
(defun winsock-init ()
  (wsa-startup)
  (let ((sock (open-socket)))
    (get-accept-ex sock)
    (get-get-addrs-ex sock)
    (get-connect-ex sock)
    (get-disconnect-ex sock)
    (close-socket sock)))

(winsock-init)



