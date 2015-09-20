

(defpackage #:cliocp.throughput
  (:use #:cl #:cliocp))

(in-package #:cliocp.throughput)

(defun run-server ()
  (with-nexus (nexus)
    (let ((f (open-file "\\\\.\\PhysicalDrive2" :disposition :open-existing)))
;;	  (seq (nibbles:make-octet-vector 4096)))
      (register-device nexus f)
      (schedule-request nexus f 'read-request)
      (do ((i 0 (1+ i)))
	  ((= i 100000))
	(handler-case (multiple-value-bind (dev req nbytes) (get-completion-result nexus 15.0)
			(declare (ignore nbytes))
			(etypecase* (dev req)
			  ((file read-request)
			   (release-request nexus req)
			   (schedule-request nexus f 'read-request))))
	  (completion-timeout (e) (declare (ignore e)) nil)
	  (completion-interrupted (e) (declare (ignore e)) nil)
	  (io-error (e) (format t "I/O error: ~A~%" e))
	  (error (e) (format t "General error: ~A~%" e)))))))

  
