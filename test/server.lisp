
(in-package #:cliocp)

;; new generic function to process request completions 
(defgeneric process-completion (nexus device request result))

;; recv 
(defmethod process-completion ((nexus nexus) (s connected-socket) (r recv-request) (nbytes integer))
  (cond
    ((zerop nbytes)
     (format t "Graceful closure~%")
     (unregister-device nexus s))
    (t 
     (format t "Recv completed ~A~%" nbytes)
     (drain-buffer (request-foreign r) (io-request-seq r) :end nbytes)     
     (format t "~X~%" (subseq (io-request-seq r) 0 nbytes))
     (schedule-request nexus s (make-instance 'send-request 
					      :seq 
					      (io-request-seq r)
					      :end nbytes)))))

;; send 
(defmethod process-completion ((nexus nexus) (s connected-socket) (r send-request) (nbytes integer))
  (format t "Send completed ~A~%" nbytes)
  (schedule-request nexus s (make-instance 'recv-request :seq (io-request-seq r) :end nbytes)))

;; accept 
(defmethod process-completion ((nexus nexus) (l listening-socket) (r accept-request) (nbytes integer))
  (format t "Accept completed~%")
  (schedule-request nexus (accept-request-conn r) 
		    (make-instance 'recv-request 
				   :seq (nibbles:make-octet-vector 4096)))
  ;; schedule another accept 
  (schedule-request nexus l (make-instance 'test-accept-request)))
				   
;; errors
(defmethod process-completion ((nexus nexus) (l connected-socket) (r recv-request) (e io-error))
  (format t "Recv error: ~A~%" e)
  (unregister-device nexus l))
(defmethod process-completion ((nexus nexus) (l connected-socket) (r send-request) (e io-error))
  (format t "Send error: ~A~%" e)
  (unregister-device nexus l))


;; wrapper for accept request
(defclass test-accept-request (accept-request)
  ())

(defmethod nexus-schedule-request ((nexus nexus) (l listening-socket) (r test-accept-request))
  (let ((c (open-connected-socket nil)))
    (register-device nexus c)
    (setf (accept-request-conn r) c)
    (call-next-method)))
  
(defun test6 (&key (timeout 15.0))
  (with-nexus (nexus)
    (let ((s (open-listening-socket :port 2000)))
      (register-device nexus s)
      (schedule-request nexus s (make-instance 'test-accept-request)))
    (do ((i 0 (1+ i)))
	((>= i 5))
      (handler-case (multiple-value-bind (dev req nbytes) (get-completion-result nexus timeout)
		      (process-completion nexus dev req nbytes)
		      (release-request nexus req))
	(completion-timeout () (format t "Timeout~%"))
	(completion-error (e) (format t "Completion error: ~A~%" e))
	(io-error (e) (process-completion nexus (io-error-dev e) (io-error-req e) e))
	(error (e) (format t "Error: ~A~%" e))))))

