;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:cliocp)

;; HANDLE WINAPI CreateWaitableTimer(
;;   _In_opt_ LPSECURITY_ATTRIBUTES lpTimerAttributes,
;;   _In_     BOOL                  bManualReset,
;;   _In_opt_ LPCTSTR               lpTimerName
;; );
(defcfun (%create-waitable-timer "CreateWaitableTimerA" :convention :stdcall) 
    :pointer
  (attrs :pointer)
  (manual :boolean)
  (name :pointer))

(defun create-waitable-timer (&optional manual-reset)
  (%create-waitable-timer (null-pointer) manual-reset (null-pointer)))

;; BOOL WINAPI SetWaitableTimer(
;;   _In_           HANDLE           hTimer,
;;   _In_     const LARGE_INTEGER    *pDueTime,
;;   _In_           LONG             lPeriod,
;;   _In_opt_       PTIMERAPCROUTINE pfnCompletionRoutine,
;;   _In_opt_       LPVOID           lpArgToCompletionRoutine,
;;   _In_           BOOL             fResume
;; );
(defcfun (%set-waitable-timer "SetWaitableTimer" :convention :stdcall)
    :boolean
  (handle :pointer)
  (due-time :pointer)
  (period :int32)
  (cb :pointer)
  (arg :pointer)
  (resume :boolean))

(defun set-waitable-timer (handle time-from-now &key period cb arg resume)
  ;; we need to provide the due-time in 100 nanoseconds. negative values indicate time from now,
  ;; which is all we're going to support for the moment 
  (with-foreign-object (l :int64)
    (setf (mem-aref l :int64) 
	  (- (floor (* time-from-now 1000 1000 10))))
    (%set-waitable-timer handle 
			 l 
			 (if period
			     (floor (* period 1000))
			     0)
			 (or cb (null-pointer))
			 (or arg (null-pointer))
			 resume)))


;; BOOL WINAPI CancelWaitableTimer(
;;   _In_ HANDLE hTimer
;; );
(defcfun (cancel-waitable-timer "CancelWaitableTimer" :convention :stdcall) :boolean
  (handle :pointer))


(defclass timer (device)
  ())

(defun open-timer (&optional manual-reset)
  (let ((h (create-waitable-timer manual-reset)))
    (make-instance 'timer :handle h)))

(defun close-timer (timer)
  (let ((h (device-handle timer)))
    (cancel-waitable-timer h)
    (close-handle h)))

(defmethod close-device ((d timer))
  (close-timer d))

(defvar *timer-requests* nil)

(defun cancel-timer (timer)
  ;; remove all requests associated with this timer
  (setf *timer-requests* 
	(remove timer *timer-requests* :key #'third))
  ;; canel it
  (cancel-waitable-timer (device-handle timer)))

(defclass timer-request (request)
  ((time-from-now :initarg :time-from-now :reader timer-request-from-now)))

(defcallback (%timer-callback :convention :stdcall) :void
    ((arg :pointer) (time-low :uint32) (time-high :uint32))
  (declare (ignore time-low time-high))
  (let ((key (pointer-address arg)))
    (format t "Timer callback Key: ~A~%" key)
    (let ((rdata (assoc key *timer-requests*)))
      (cond
	(rdata
	 (handler-case 
	     (destructuring-bind (nexus dev req) (cdr rdata)
	       (init-foreign (request-foreign req))
	       (iocp-post-status (nexus-iocp nexus)
				 (device-key dev)
				 0
				 (foreign-overlapped (request-foreign req)))
	       (setf *timer-requests* (remove rdata *timer-requests*)))
	   (error (e) (format t "Failed to post: ~A~%" e))))
	(t 
	 (format t "RDATA not found~%"))))))
			    

(defmethod nexus-schedule-request ((nexus nexus) (d timer) (r timer-request))
  (push (list (request-id r) nexus d r) *timer-requests*)
  (set-waitable-timer (device-handle d)
		      (timer-request-from-now r)
		      :cb (callback %timer-callback)
		      :arg (make-pointer (request-id r))))


