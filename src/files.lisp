;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines an API for operating on filesystem file handles. The low-level functions are 
;;; already defined for us by POUNDS, we just use those functions.

(in-package #:cliocp)

(defconstant +FILE-FLAG-NO-BUFFERING+ #x20000000)
(defconstant +FILE-FLAG-OVERLAPPED+ #x40000000)
(defconstant +FILE-FLAG-WRITE-THROUGH+ #x80000000)

(defun maybe-get-last-error* ()
  (let ((code (pounds::%get-last-error)))
    (case code
      ((0 997) ;; ERROR_IO_PENDING
       nil)
      (otherwise 
       (error 'pounds::win-error :code code)))))

(defclass file (device)
  ())

(defun open-file (path &key (disposition :open-always))
  (with-foreign-string (str path)
    (let ((handle (pounds::%create-file str 
					#x10000000 ;; GENERIC_ALL ;; #xC0000000 ;; access == generic_read|generic_write
					3 ;; mode == share_read|share_write
					(null-pointer)
					(ecase disposition
					  (:create-new 1)
					  (:create-always 2)
					  (:open-existing 3)
					  (:open-always 4)
					  (:truncate-existing 5))
					+FILE-FLAG-OVERLAPPED+
					(null-pointer))))
      (when (pounds::invalid-handle-p handle)
	(pounds::get-last-error))
      (make-instance 'file :handle handle))))

(defmethod close-device ((f file))
  (close-handle (device-handle f)))



;; I/O requests

(defclass file-io-request (io-request)
  ((offset :initarg :offset :initform 0 :reader file-io-request-offset)))

(defclass read-request (file-io-request)
  ())

(defmethod nexus-schedule-request ((nexus nexus) (f file) (r read-request))
  (with-foreign-object (nbytes :uint32)
    (let ((count (- (or (io-request-end r) (length (io-request-seq r)))
		    (io-request-start r))))
      (init-foreign (request-foreign r)
		    :offset (file-io-request-offset r)
		    :count count)
      (let ((res (pounds::%read-file (device-handle f)
				     (foreign-buffer (request-foreign r))
				     count 
				     nbytes
				     (foreign-overlapped (request-foreign r)))))
	(if res
	    nil
	    (maybe-get-last-error*))))))


(defclass write-request (file-io-request)
  ())

(defmethod nexus-schedule-request ((nexus nexus) (f file) (r write-request))
  (with-foreign-object (nbytes :uint32)
    (let ((count (- (or (io-request-end r) (length (io-request-seq r)))
		    (io-request-start r))))
      (init-foreign (request-foreign r)
		    :offset (file-io-request-offset r)
		    :count count)
      (fill-buffer (request-foreign r) 
		   (io-request-seq r)
		   :start (io-request-start r)
		   :end (io-request-end r))
      (let ((res (pounds::%write-file (device-handle f)
				      (foreign-buffer (request-foreign r))
				      count 
				      nbytes
				      (foreign-overlapped (request-foreign r)))))
	(if res
	    nil
	    (maybe-get-last-error*))))))

;; Could add: LockFileEx, UnlockFileEx
