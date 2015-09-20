;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:cliocp)

;; ----------- Client TCP connections -----------------

(defclass connected-socket (device)
  ())

(defun open-connected-socket (&optional (bind t))
  (let ((s (open-socket)))
    (when bind (socket-bind s))
    (make-instance 'connected-socket :handle s)))

(defun close-connected-socket (c)
  (close-socket (device-handle c)))

(defmethod close-device ((c connected-socket))
  (close-connected-socket c))

;; operations 
(defclass connect-request (request)
  ((addr :initarg :addr :reader connect-request-addr)
   (port :initarg :port :reader connect-request-port)))

(defmethod nexus-schedule-request ((nexus nexus) (c connected-socket) (r connect-request))
  (init-foreign (request-foreign r))
  (connect-ex (device-handle c)
              (connect-request-addr r)
              (connect-request-port r)
              (foreign-overlapped (request-foreign r))))

(defclass disconnect-request (request)
  ((reuse :initform nil :initarg :reuse :reader disconnect-request-reuse)))

(defmethod nexus-schedule-request ((nexus nexus) (c connected-socket) (r disconnect-request))
  (init-foreign (request-foreign r))
  (disconnect-ex (device-handle c)
                 (foreign-overlapped (request-foreign r))
                 (disconnect-request-reuse r)))




(defclass io-request (request)
  ((seq :initform nil :initarg :seq :reader io-request-seq)
   (start :initform 0 :initarg :start :reader io-request-start)
   (end :initform nil :initarg :end :reader io-request-end)))

(defclass send-request (io-request)
  ())

(defmethod nexus-schedule-request ((nexus nexus) (c connected-socket) (r send-request))
  (init-foreign (request-foreign r) 
                :count (- (or (io-request-end r) (length (io-request-seq r)))
                          (io-request-start r)))
  (fill-buffer (request-foreign r) 
               (io-request-seq r)
               :start (io-request-start r)
               :end (io-request-end r))
  (wsa-send (device-handle c)
            (foreign-wsabuf (request-foreign r))
            (foreign-overlapped (request-foreign r))))

(defclass recv-request (io-request)
  ())

(defmethod nexus-schedule-request ((nexus nexus) (c connected-socket) (r recv-request))
  (init-foreign (request-foreign r) 
                :count (- (or (io-request-end r) (length (io-request-seq r)))
                          (io-request-start r)))
  (wsa-recv (device-handle c)
            (foreign-wsabuf (request-foreign r))
            (foreign-overlapped (request-foreign r))))

;; ---------------- Listening sockets ------------------

(defclass listening-socket (device)
  ())

(defun open-listening-socket (&key port address backlog)
  (let ((s (open-socket)))
    (socket-bind s port address)
    (socket-listen s backlog)
    (make-instance 'listening-socket :handle s)))

(defun close-listening-socket (s)
  (close-socket (device-handle s)))

(defmethod close-device ((s listening-socket))
  (close-listening-socket s))

;; operations
(defclass accept-request (request)
  ((conn :initarg :conn :accessor accept-request-conn)))

(defmethod nexus-schedule-request ((nexus nexus) (l listening-socket) (r accept-request))
  (init-foreign (request-foreign r))
  (accept-ex (device-handle l)
	     (device-handle (accept-request-conn r))
	     (foreign-buffer (request-foreign r))
	     (foreign-overlapped (request-foreign r))))
