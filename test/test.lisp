
(in-package #:cliocp)

;; First version, does lots of low level calls 
(defun test ()
  (let* ((iocp-h (open-iocp))
         (sock (open-socket)))
    (unwind-protect 
         (progn 
           (iocp-associate iocp-h sock 0) ;; listenign socket KEY == 0
           (socket-bind sock 2000) ;; bind() socket 
           (socket-listen sock) ;; listen() socket 
           
           (let ((conn (open-socket)))
             (unwind-protect 
                  (progn 
                    (iocp-associate iocp-h conn 1) ;; connection socket KEY == 1
                    
                    ;; schedule an accept 
                    (with-foreign-objects ((buffer :uint8 4096)
                                           (overlapped '(:struct pounds::overlapped))
                                           (wsabuf '(:struct wsabuf)))
                      (memset overlapped '(:struct pounds::overlapped) 0)
                      (accept-ex sock conn buffer overlapped)
                      (do ((i 0 (1+ i))
                           (state nil))
                          ((= i 12))
                        (multiple-value-bind (key nb code) (iocp-get-status iocp-h 15.0)
                          (format t "KEY: ~S nbytes: ~S code: ~S~%" key nb code)
                          (case key 
                            (0 ;; listening socket accepted a connection
                             (format t "Accepted connection~%")
                             ;; get the addresses 
                             (multiple-value-bind (local-addr local-port remote-addr remote-port) 
                                 (get-addrs-ex buffer)
                               (format t "LOCAL: ~A:~A REMOTE: ~A:~A~%" 
                                       local-addr local-port remote-addr remote-port))
                             ;; schedule a read
                             (format t "Scheduling READ~%")
                             (init-wsabuf wsabuf buffer 4096)
                             (wsa-recv conn wsabuf overlapped)
                             (setf state :read))
                            (1 ;; connection I/O
                             ;; schedule a write
                             (case state 
                               (:write
                                (format t "WRITE completed. Scheduling READ~%")
                                (init-wsabuf wsabuf buffer 4096)
                                (wsa-recv conn wsabuf overlapped)
                                (setf state :read))
                               (:read
                                (cond
                                  ((zerop nb)
                                   (format t "Connection closed gracefully, disonnecting~%")
                                   (disconnect-ex conn overlapped)
                                   (setf state :disconnect))
                                  (t 
                                   (format t "READ completed. Scheduling WRITE~%")
                                   (init-wsabuf wsabuf buffer nb)
                                   (wsa-send conn wsabuf overlapped)
                                   (setf state :write))))
                               (:disconnect 
                                (format t "Connection disconnectd, accepting new connection~%")
                                (accept-ex sock conn buffer overlapped)))))))))
               (close-socket conn))))
      (close-handle iocp-h)
      (close-socket sock))))

(defun test2 ()
  (let ((iocp (open-iocp))
        (sock (open-socket))
        (conn (open-socket))	  
        (foreign (allocate-foreign 0))) 
    (iocp-associate iocp sock 0)
    (iocp-associate iocp conn 1)
    (socket-bind sock 2000)
    (socket-listen sock)
    (unwind-protect 
         (progn
           (init-foreign foreign)
           (accept-ex sock conn (foreign-buffer foreign) (foreign-overlapped foreign))
           (do ((i 0 (1+ i))
                (state nil))
               ((>= i 5))
             (multiple-value-bind (key nbytes error-code req-id) (iocp-get-status iocp 15)
               (format t "KEY: ~A ID: ~A NBYTES: ~A CODE: ~A~%" key req-id nbytes error-code)
               (case key 
                 (0 
                  ;; accepted connection, get addresses
                  (multiple-value-bind (raddr rport) (get-addrs-ex (foreign-buffer foreign))
                    (format t "Accepted from ~S:~S~%" raddr rport))
                  ;; schedule read
                  (init-foreign foreign)
                  (wsa-recv conn (foreign-wsabuf foreign) (foreign-overlapped foreign))
                  (setf state :read))
                 (1 
                  (case state
                    (:read 
                     ;; read completed
                     (format t "READ nbytes=~A~%" nbytes)
                     (cond
                       ((and (zerop nbytes) (eq state :read))
                        (format t "Graceful socket close~%")
                        (setf i 10))
                       (t 
                        (let ((seq (nibbles:make-octet-vector nbytes)))
                          (drain-buffer foreign seq)
                          (format t "~S~%" seq)
                          (fill-buffer foreign seq)
                          (init-foreign foreign :count nbytes)
                          (wsa-send conn (foreign-wsabuf foreign) (foreign-overlapped foreign))
                          (setf state :write)))))
                    (:write 
                     (format t "WRITE nbytes=~A~%" nbytes)
                     (init-foreign foreign)
                     (wsa-recv conn (foreign-wsabuf foreign) (foreign-overlapped foreign))
                     (setf state :read))))))))			      
      (close-iocp iocp)
      (close-socket sock)
      (close-socket conn)
      (free-foreign foreign))))

;; (defun test3 ()
;;   (with-nexus (nexus)
;;     (let ((s (open-device nexus (make-instance 'listening-socket :port 2000))))
;;       (schedule-request nexus s (make-instance 'accept-request))
;;       (do ((i 0 (1+ i)))
;;           ((>= i 5))
;;         (multiple-value-bind (dev req res) (get-completion-result nexus 15.0)
;;           (etypecase dev 
;;             (listening-socket 
;;              (etypecase req 
;;                (accept-request (destructuring-bind (conn &key remote-address remote-port error-code &allow-other-keys) res
;;                                  (cond
;;                                    ((and error-code (not (zerop error-code)))
;;                                     (format t "Error accepting: ~A~%" error-code))
;;                                    ((or (not error-code) (zerop error-code)) 
;;                                     (format t "Accepted from ~A:~A~%" remote-address remote-port)
;;                                     (schedule-request nexus conn (make-instance 'recv-request))))))))
;;             (connected-socket 
;;              (etypecase req 
;;                (recv-request (destructuring-bind (error-code seq nbytes) res 
;;                                (cond
;;                                  (error-code 
;;                                    (format t "Recv error: ~A~%" error-code))
;;                                  ((zerop nbytes)
;;                                   (format t "Connection closed~%")
;;                                   (close-device nexus dev))
;;                                  (t 
;;                                   (format t "Recv: ~A~%" seq)
;;                                   (schedule-request nexus dev 
;;                                                     (make-instance 'send-request :seq seq :end nbytes))))))
;;                (send-request (destructuring-bind (error-code nbytes) res 
;;                                (if error-code 
;;                                    (format t "Send error: ~A~%" error-code)
;;                                    (format t "Send ~A~%" nbytes))
;;                                (schedule-request nexus dev (make-instance 'recv-request))))))))))))

	       
;; --------------------
			      

(defun test4 ()
  (with-nexus (nexus)
    (let ((s (open-listening-socket :port 2000))
	  (c (open-connected-socket nil)))
      (register-device nexus s)
      (register-device nexus c)
      (schedule-request nexus s 'accept-request :conn c))
    (do ((seq (nibbles:make-octet-vector 4096))
	 (i 0 (1+ i)))
	((>= i 5))
      (handler-case (multiple-value-bind (dev req nbytes) (get-completion-result nexus 15.0)
		      (etypecase* (dev req)
		        ((listening-socket accept-request) ;; accpeted a connection
			 (let ((c (accept-request-conn req)))
			   (schedule-request nexus c (make-instance 'recv-request :seq seq))))
			((connected-socket recv-request) ;; received data from connection
			 (cond
			   ((zerop nbytes)
			    (format t "Graceful close~%")
			    (unregister-device nexus dev)
			    (setf i 10))
			   (t 
			    (drain-buffer (request-foreign req) seq :end nbytes)
			    (format t "Recv ~A bytes~%~A~%" nbytes (subseq seq 0 nbytes))
			    ;; echo back
			    (schedule-request nexus dev 'send-request :seq seq :end nbytes))))
			((connected-socket send-request) ;; send completed
			 (format t "Sent ~A bytes~%" nbytes)
			 (schedule-request nexus dev 'recv-request :seq seq)))
		      (release-request nexus req))
	(completion-timeout (e) (declare (ignore e)) (format t "Completion timeout~%"))
	(completion-error (e) (format t "Completion error: ~A~%" e))
	(io-error (e) 
	  (format t "I/O error: ~A~%" e)
	  (unregister-device nexus (io-error-dev e)))
	(error (e) (format t "General error: ~A~%" e))))))

;; shows how to use the timers
(defun test5 ()
  (with-nexus (nexus)
    (let ((timer (open-timer)))
      (register-device nexus timer :associate nil)
      (schedule-request nexus timer (make-instance 'timer-request :time-from-now 5))
      (do ((i 0 (1+ i)))
	  ((>= i 5))
	(handler-case 
	    (multiple-value-bind (dev req nbytes) (get-completion-result nexus 15.0 t)
	      (declare (ignore nbytes))
	      (etypecase* (dev req)
	        ((timer timer-request)
		 (format t "Timer triggered~%")
		 (release-request nexus req)
		 (schedule-request nexus dev 'timer-request :time-from-now (1+ i)))))
	  (completion-timeout (e) (declare (ignore e)) (format t "Completion timeout~%"))
	  (completion-interrupted (e) (declare (ignore e)) nil)
	  (completion-error (e) (format t "Completion error: ~A~%" e))
	  (io-error (e) 
	    (format t "I/O error: ~A~%" e)
	    (unregister-device nexus (io-error-dev e)))
	  (error (e) (format t "General error: ~A~%" e)))))))
  



;; ----------------
