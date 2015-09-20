;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:cliocp)

;; global (toplevel) state
;; includes iocp handle, freelist of foreign data etc
(defclass nexus ()
  ((iocp :initarg :iocp :accessor nexus-iocp)
   (key :initform 0 :accessor nexus-key)
   (devices :initform nil :accessor nexus-devices)
   (id :initarg :id :initform 0 :accessor nexus-id)
   (buffer-size :initarg :buffer-size :initform +buffer-size+ :reader nexus-buffer-size)
   (freelist :initform nil :initarg :freelist :accessor nexus-freelist)
   (all-foreign :initform nil :accessor nexus-all-foreign)
   (requests :initform nil :accessor nexus-requests)))

(defmethod initialize-instance :after ((nexus nexus) &rest initargs &key nthreads buffer-count &allow-other-keys)
  (declare (ignore initargs))
  (let* ((bcount (or buffer-count 32))
	 (f (loop :for i :below bcount 
	       :collect (allocate-foreign i (nexus-buffer-size nexus)))))
    (setf (nexus-iocp nexus) (open-iocp (or nthreads 0))
          (nexus-freelist nexus) f
          (nexus-all-foreign nexus) f
          (nexus-id nexus) bcount)))
        
(defun open-nexus ()
  (make-instance 'nexus))

(defun close-nexus (nexus)
  (declare (type nexus nexus))

  ;; close all devices
  (dolist (d (nexus-devices nexus))
    (unregister-device nexus d))

  ;; close iocp handle
  (close-iocp (nexus-iocp nexus))

  ;; free all foreign 
  (dolist (f (nexus-all-foreign nexus))
    (free-foreign f)))
            
(defun acquire-foreign (nexus)
  (declare (type nexus nexus))
  (cond
    ((nexus-freelist nexus)
     (pop (nexus-freelist nexus)))
    (t 
     (incf (nexus-id nexus))
     (let ((f (allocate-foreign (nexus-id nexus) (nexus-buffer-size nexus))))
       (push f (nexus-all-foreign nexus))
       f))))

(defun release-foreign (nexus f)
  (declare (type nexus nexus)
	   (type foreign f))
  (push f (nexus-freelist nexus)))
  
;; --------------------------------------

(defclass device ()
  ((key :initform nil :accessor device-key)
   (handle :initarg :handle :reader device-handle)))

(defgeneric close-device (device))

(defun register-device (nexus device &key (associate t))
  (declare (type nexus nexus)
	   (type device device))
  ;; allocate this device a key
  (incf (nexus-key nexus))
  (let ((key (nexus-key nexus)))
    ;; associate with IOCP
    (when associate 
      (iocp-associate (nexus-iocp nexus) (device-handle device) key))
    (setf (device-key device) key)
    ;; store in assoc list 
    (push device (nexus-devices nexus)))
  device)

(defun unregister-device (nexus device-or-key)
  (declare (type nexus nexus)
	   (type (or integer device) device-or-key))
  (etypecase device-or-key
    (integer 
     (let ((d (find-device nexus device-or-key)))
       (close-device d)
       (setf (nexus-devices nexus)
	     (remove d (nexus-devices nexus) :test #'eq))))
    (device 
     (close-device device-or-key)
     (setf (nexus-devices nexus)
           (remove device-or-key (nexus-devices nexus)
                   :test #'eq)))))

(defun find-device (nexus key-or-type)
  (declare (type nexus nexus)
	   (type (or integer symbol) key-or-type))
  (etypecase key-or-type
    (integer (find key-or-type (nexus-devices nexus)
                   :test #'= :key #'device-key))
    (symbol (find-if (lambda (dev)
                       (typep dev key-or-type))
                     (nexus-devices nexus)))))
     


;; ---------------------------------------



(defclass request ()
  ((foreign :initform nil :accessor request-foreign)))

(defun request-id (request)
  (declare (type request request))
  (foreign-id (request-foreign request)))

(defgeneric nexus-schedule-request (nexus device request))

(defun schedule-request (nexus device request &rest initargs)
  (declare (type nexus nexus)
	   (type device device)
	   (type (or request symbol class) request))
  (let ((r (etypecase request 
	     (request request)
	     (symbol (apply #'make-instance request initargs))
	     (class (apply #'make-instance request initargs)))))
    ;; acquire a foreign and provide it to the request 
    (let ((f (acquire-foreign nexus)))
      (setf (request-foreign r) f)
      
      ;; run the custom code to call the relevant underlying API (WriteFile, WSASend etc.)
      (nexus-schedule-request nexus device r)
      
      ;; push onto the request list so we can get it later 
      (push r (nexus-requests nexus)))
    r))

(defun get-completion-result (nexus &optional timeout alertable)
  (declare (type nexus nexus)
	   (type (or null number) timeout))
  ;; call GetQueuedCompletionStatus, find the assocaited device and request, 
  ;; then pass those to completion-result, finally return (values device request result)
  (multiple-value-bind (key nbytes error-code req-id) (if alertable
							  (iocp-get-status-ex (nexus-iocp nexus) timeout alertable)
							  (iocp-get-status (nexus-iocp nexus) timeout))
    (let ((dev (find-device nexus key))
          (req (find req-id (nexus-requests nexus)
                     :test #'= :key #'request-id)))
      (unless dev (error "Device not found"))
      (unless req (error "Request not found"))

      ;; remove the request from the pending list 
      (setf (nexus-requests nexus) 
            (remove req (nexus-requests nexus)))

      ;; if non-zero error code then release foreign and signal error
      (unless (zerop error-code)
	(release-foreign nexus (request-foreign req))
	(setf (request-foreign req) nil)
	(error 'io-error 
	       :code error-code
	       :dev dev
	       :req req))

      (values dev req nbytes))))

(defun release-request (nexus request)
  (declare (type nexus nexus)
	   (type request request))
  (release-foreign nexus (request-foreign request)))

(defmacro with-nexus ((var &optional form) &body body)
  `(let ((,var ,(or form '(open-nexus))))
     (declare (type nexus ,var))
     (unwind-protect (progn ,@body)
       (close-nexus ,var))))




(defmacro etypecase* (keyforms &rest clauses)
  (if (cdr keyforms)
      ;; there are more forms to evaluate yet, we need to collect all those clauses of the same initial type
      (let ((first-types (remove-duplicates (mapcar #'caar clauses)))
	    (gvars (mapcar (lambda (form) (declare (ignore form)) (gensym)) keyforms)))
	`(let (,@(mapcar (lambda (gvar form) 
			   `(,gvar ,form))
			 gvars keyforms))
	   (etypecase ,(car gvars)
	   ,@(mapcar (lambda (type1)
		       `(,type1 (etypecase* ,(cdr gvars)
				  ,@(mapcan (lambda (clause)
					      (destructuring-bind (types &body body) clause
						(when (eq (car types) type1)
						  `((,(cdr types) ,@body)))))
					    clauses))))
		     first-types))))
      `(etypecase ,(car keyforms)
	 ,@(mapcar (lambda (clause)
		     (destructuring-bind (types &body body) clause
		       `(,(car types) ,@body)))
		   clauses))))
			


