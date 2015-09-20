;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines the wrappers for the I/O completion ports API.

(in-package #:cliocp)

;; HANDLE WINAPI CreateIoCompletionPort(
;;   _In_     HANDLE    FileHandle,
;;   _In_opt_ HANDLE    ExistingCompletionPort,
;;   _In_     ULONG_PTR CompletionKey,
;;   _In_     DWORD     NumberOfConcurrentThreads
;; );
(defcfun (%create-iocp "CreateIoCompletionPort" :convention :stdcall)
    :pointer
  (fh :pointer)
  (existing :pointer)
  (key :pointer)
  (nthreads :uint32))

(defun open-iocp (&optional (nthreads 0))
  "Open a new IO completion port."
  (let ((handle (%create-iocp (make-pointer #+(or x64 x86-64 amd64)#xffffffffffffffff
					    #-(or x86 x86-64 amd64)#xffffffff)
			      (null-pointer)
			      (null-pointer)
			      nthreads)))
    (if (null-pointer-p handle)
	(pounds::get-last-error)
	handle)))

(defun close-iocp (handle)
  "Close the IO completion port."
  (pounds::%close-handle handle))

(defun iocp-associate (iocp-handle handle key)
  "Associate a handle with the IOCP.

HANDLE ::= a file handle 
VALUE ::= a value to associate with the handle.
KEY ::= a integer key to use. If not supplied a default will be used.
"
  (let ((h (%create-iocp handle 
			 iocp-handle
			 (make-pointer key)
			 0)))
    (when (null-pointer-p h)
      (pounds::get-last-error))
    nil))

;;(defconstant +ERROR-TIMEOUT+ 1460)
(defconstant +wait-timeout+ 258
  "Indicates the operation timed out.")
(defconstant +wait-io-completion+ 192
  "Indicates the operation was interrupted by an APC.")

;; The OVERLAPPED structure represents "per-call" data, i.e. data that is associated with a 
;; single I/O request. This is in contrast with the IOCP "key" which represents the handle to 
;; which the I/O is associated. 
;; Therefore, if we want to issue multiple I/Os on the same handle we need to somehow tag each OVERLAPPED
;; structure with an ID so we can match up the completion packet with the associated I/O request.
;; This is easy enough to do, we just add a bit of data to the end of the overlapped struct 

;; We want to extend this functionality so we can map I/O requests to completion packets
;; We do this by extending the OVERLAPPED structure with a uint32 to identify the I/O request 
(defcstruct overlapped* 
  (overlapped (:struct pounds::overlapped))
  (id :uint32))

(defun memset (ptr foreign-type &optional (value 0))
  (dotimes (i (foreign-type-size foreign-type))
    (setf (mem-ref ptr :uint8 i) value)))

(defun init-overlapped (ptr id &optional offset)
  (memset ptr '(:struct overlapped*))
  (when offset 
    (let ((p (inc-pointer ptr (foreign-slot-offset '(:struct overlapped*) 'overlapped))))
      (multiple-value-bind (low high) (pounds::split-offset offset)
	(setf (foreign-slot-value p '(:struct pounds::overlapped) 'pounds::offset)
	      low
	      (foreign-slot-value p '(:struct pounds::overlapped) 'pounds::offset-high)
	      high))))
  (setf (foreign-slot-value ptr '(:struct overlapped*) 'id) id)
  ptr)

;; BOOL WINAPI GetQueuedCompletionStatus(
;;   _In_  HANDLE       CompletionPort,
;;   _Out_ LPDWORD      lpNumberOfBytes,
;;   _Out_ PULONG_PTR   lpCompletionKey,
;;   _Out_ LPOVERLAPPED *lpOverlapped,
;;   _In_  DWORD        dwMilliseconds
;; );
(defcfun (%get-queued-completion-status "GetQueuedCompletionStatus" :convention :stdcall)
    :boolean
  (handle :pointer)
  (nbytes :pointer)
  (key :pointer)
  (overlapped :pointer)
  (milli :uint32))

;; typedef struct _OVERLAPPED_ENTRY {
;;   ULONG_PTR    lpCompletionKey;
;;   LPOVERLAPPED lpOverlapped;
;;   ULONG_PTR    Internal;
;;   DWORD        dwNumberOfBytesTransferred;
;; }
(defcstruct overlapped-entry 
  (key :pointer)
  (overlapped :pointer)
  (internal :pointer)
  (nbytes :uint32))

;; bool WINAPI GetQueuedCompletionStatusEx(
;;   _In_  HANDLE             CompletionPort,
;;   _Out_ LPOVERLAPPED_ENTRY lpCompletionPortEntries,
;;   _In_  ULONG              ulCount,
;;   _Out_ PULONG             ulNumEntriesRemoved,
;;   _In_  DWORD              dwMilliseconds,
;;   _In_  BOOL               fAlertable
;; );
(defcfun (%get-queued-completion-status-ex "GetQueuedCompletionStatusEx" :convention :stdcall)
    :boolean
  (handle :pointer)
  (entries :pointer)
  (count :uint32)
  (nremoved :pointer)
  (milli :uint32)
  (alertable :boolean))
(defun iocp-get-status-ex (handle &optional timeout alertable)
  (with-foreign-objects ((entries '(:struct overlapped-entry))
			 (nremoved :uint32))
    (let ((b (%get-queued-completion-status-ex handle 
					       entries
					       1
					       nremoved
					       (if timeout 
						   (floor (* timeout 1000))
						   #xffffffff)
					       alertable)))
      (cond 
	((not b)
	 (let ((error-code (pounds::%get-last-error)))
	   ;; failed to dequeue a completion packet
	   (cond
	     ((= error-code +wait-timeout+) 
	      ;; timeout occured, just return nil
	      (signal (make-condition 'completion-timeout))
	      nil)
	     ((= error-code +wait-io-completion+)
	      ;; we were interrupted by an APC, just call ourselves again? Or signal upwards?
;;	      (iocp-get-status-ex handle timeout alertable))
	      (signal (make-condition 'completion-interrupted)))
	     (t 
	      (error 'completion-error :code error-code)))))
	(t 
	 (let ((n (mem-aref nremoved :uint32)))
	   (if (zerop n)
	       (error "none deququed")
	       ;; successfully dequeued a completion packet, get the data from the overlapped 
	       (let ((overlapped (foreign-slot-value entries '(:struct overlapped-entry) 'overlapped))
		     (key (pointer-address (foreign-slot-value entries '(:struct overlapped-entry) 'key)))
		     (nbytes (foreign-slot-value entries '(:struct overlapped-entry) 'nbytes)))
		 (let ((error-code (pointer-address 
				    (foreign-slot-value overlapped 
							'(:struct pounds::overlapped) 
							'pounds::internal))))
		   (values key
			   nbytes
			   error-code
			   (unless (null-pointer-p overlapped) ;; post-completion-status can pass a null-pointer
			     (foreign-slot-value overlapped '(:struct overlapped*) 'id))))))))))))



;; There are 3 cases to handle:
;; 1. There was an error dequeuing the packet (e.g. someone closed the iocp handle).
;; In this case we signal an error.
;; 2. An I/O completion packet was dequeued, but the I/O failed. We return
;; (values key nbytes error-code nil) in this scenario.
;; 3. An I/O completion packet was dequeued and the I/O succeeded. We return
;; (values key nbytes 0 overlapped) where overlapped is a pointer to the associated OVERLAPPED struct
;; that was provided in the I/O request. 
(defun iocp-get-status (handle &optional timeout)
  "Wait for up to TIMEOUT seconds for the next completion packet to arrive.

HANDLE ::= iocp handle as returned from OPEN-IOCP
TIMEOUT ::= number of seconds to wait before timing out. If not supplied waits indefinitely. If the call
times out then it returns (values nil nil nil nil).

Returns (values key nbytes error-code overlapped).
"
  (with-foreign-objects ((nbytes :uint32)
                         (key-pointer :pointer)
                         (overlapped-pointer :pointer))
    (let ((b (%get-queued-completion-status handle
                                            nbytes
                                            key-pointer 
                                            overlapped-pointer
                                            (if timeout 
                                                (floor (* timeout 1000))
                                                #xFFFFFFFF))))
      (let ((overlapped (mem-ref overlapped-pointer :pointer))
            (key (pointer-address (mem-ref key-pointer :pointer))))
        (cond 
          ((not b)
           (let ((error-code (pounds::%get-last-error)))
             (cond
               ((null-pointer-p overlapped)
                ;; failed to dequeue a completion packet
                (cond
                  ((= error-code +wait-timeout+) 
                   ;; timeout occured, just return nil
                   (signal (make-condition 'completion-timeout))
                   nil)
                  (t 
                   (error 'completion-error :code error-code))))
               (t 
                ;; dequeued a failed completion packet 
                (values key
                        (mem-aref nbytes :uint32)
                        error-code
                        (foreign-slot-value overlapped '(:struct overlapped*) 'id))))))
          (t 
           ;; successfully dequeued a completion packet 
           (values key
                   (mem-aref nbytes :uint32)
                   0
                   (unless (null-pointer-p overlapped) ;; post-completion-status can pass a null-pointer
                     (foreign-slot-value overlapped '(:struct overlapped*) 'id)))))))))
	 

;; BOOL WINAPI PostQueuedCompletionStatus(
;;   _In_     HANDLE       CompletionPort,
;;   _In_     DWORD        dwNumberOfBytesTransferred,
;;   _In_     ULONG_PTR    dwCompletionKey,
;;   _In_opt_ LPOVERLAPPED lpOverlapped
;; );
(defcfun (%post-completion-status "PostQueuedCompletionStatus" :convention :stdcall)
    :boolean
  (handle :pointer)
  (nbytes :uint32)
  (key :pointer)
  (overlapped :pointer))

(defun iocp-post-status (handle key &optional (nbytes 0) overlapped)
  "Post a completion packet to the IOCP."
  (let ((b (%post-completion-status handle
				    nbytes
				    (make-pointer key)
				    (or overlapped (null-pointer)))))
    (if b
	key
	(pounds::get-last-error))))


(defmacro with-iocp ((var &optional nthreads) &body body)
  `(let ((,var (open-iocp ,@(when nthreads `(,nthreads)))))
     (unwind-protect (progn ,@body) 
       (close-iocp ,var))))



