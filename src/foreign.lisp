;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:cliocp)

;; stores the foreign data for an I/O request. We maintain a free-list of these to keep memory
;; management simpler.
(defstruct foreign
  (buffer nil :type (or null foreign-pointer))
  (count 0 :type integer)
  (ptr nil :type (or null foreign-pointer))
  (wsabuf nil :type (or null foreign-pointer))
  (overlapped nil :type (or null foreign-pointer))
  (id 0 :type integer))

;; define a smaller structure to hold all the overlapped/wsabuf structs
(defcstruct foreign-data 
  (overlapped (:struct overlapped*))
  (wsabuf (:struct wsabuf)))

(defconstant +buffer-size+ 4096)
(defun allocate-foreign (id &optional (buffer-size +buffer-size+))
  (let ((buffer (foreign-alloc :uint8 :count +buffer-size+))
	(ptr (foreign-alloc '(:struct foreign-data))))
    (make-foreign :id id 
		  :buffer buffer
		  :count buffer-size
		  :ptr ptr
		  :wsabuf (inc-pointer ptr
				       (foreign-slot-offset '(:struct foreign-data) 
							    'wsabuf))
		  :overlapped (inc-pointer ptr
					   (foreign-slot-offset '(:struct foreign-data) 
								     'overlapped)))))

(defun free-foreign (foreign)
  (foreign-free (foreign-ptr foreign))
  (foreign-free (foreign-buffer foreign)))

(defun init-foreign (foreign &key offset count)
  (init-overlapped (foreign-overlapped foreign) (foreign-id foreign) offset)
  (init-wsabuf (foreign-wsabuf foreign) 
	       (foreign-buffer foreign)
	       (or count (foreign-count foreign))))

(defun fill-buffer (foreign seq &key (start 0) end)
  (let* ((count (- (or end (length seq)) start))
	 (p (foreign-buffer foreign))
	 (c (foreign-count foreign)))
    (when (> count c)
      (error "Count ~S larger than buffer count ~S" count c))
    (dotimes (i count)
      (setf (mem-aref p :uint8 i) (aref seq (+ start i))))))

(defun drain-buffer (foreign seq &key (start 0) end)
  (let* ((count (- (or end (length seq)) start))
	 (p (foreign-buffer foreign))
	 (c (foreign-count foreign)))
    (when (> count c)
      (error "Count ~S larger than buffer count ~S" count c))
    (dotimes (i count)
      (setf (aref seq (+ start i)) (mem-aref p :uint8 i)))
    seq))

(defmacro with-foreign ((var &key id buffer-size) &body body)
  `(let ((,var (allocate-foreign ,(or id (random (1+ #xffffffff)))
				 ,@(when buffer-size 
				     `(,buffer-size)))))
     (unwind-protect (progn ,@body)
       (foreign-free ,var))))
					
(defmacro with-foreign-list ((var count &key buffer-size) &body body)
  (let ((gf (gensym "FOREIGN"))
	(gi (gensym)))
    `(let ((,var (loop :for ,gi :below ,count 
		    :collect (allocate-foreign ,gi
					       ,@(when buffer-size 
						    `(,buffer-size))))))
       (unwind-protect (progn ,@body)
	 (dolist (,gf ,var) 
	   (foreign-free ,gf))))))

