;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:cliocp)

(define-condition completion-error (pounds::win-error)
  ())

(define-condition io-error (pounds::win-error)
  ((dev :initarg :dev :reader io-error-dev)
   (req :initarg :req :reader io-error-req)))

(define-condition completion-timeout (condition)
  ())

(define-condition completion-interrupted (condition)
  ())
