

(defpackage #:cliocp.rpc
  (:use #:cl #:cliocp))

(in-package #:cliocp.rpc)

(defun run-rpc-server (server)
  (with-nexus (nexus)
    (let ((s (open-listening-socket :port (car (frpc:rpc-server-tcp-ports server))))
	  (c (open-connected-socket nil)))
      (register-device nexus s)
      (register-device nexus c)
      (schedule-request nexus s (make-instance 'accept-request :conn c)))
    (do ()
	((frpc::rpc-server-exiting server))
      (handler-case 
	  (multiple-value-bind (dev req nbytes) (get-completion-result nexus 1.0)
	    (etypecase* (dev req)
              ((listening-socket accept-request)
	       ;; scuedule a read 
	       (schedule-request nexus (accept-request-conn req) 'recv-request)
	       
	       ;; accepted a connection, schedule a recv and start accepting again
	       (let ((c (open-connected-socket nil)))
		 (register-device nexus c)
		 (schedule-request nexus dev 'accept-request :conn c)))
	      ((connected-socket recv-request)
	       ;; recv completed
	       (
	    (release-request nexus req))
	(completion-timeout (e) (declare (ignore e)) nil)
	(completion-interrupted (e) (declare (ignore e)) nil)
	(io-error (e) 
	  (format t "I/O error: ~A~%" e)
	  (unregister-device nexus (io-error-dev e)))
	(error (e) (format t "General error: ~A~%" e))))))

      
