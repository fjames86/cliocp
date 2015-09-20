;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :cliocp
  :name "cliocp"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Lisp API for Windows I/O Completion Ports."
  :license "MIT"
  :serial t
  :components
  ((:module :src
	    :pathname "src"
	    :components 
	    ((:file "package")
	     (:file "errors")
	     (:file "iocp")
	     (:file "winsock2")
	     (:file "foreign")
	     (:file "nexus")
	     (:file "connections")
	     (:file "timers")
	     (:file "files"))))
  :depends-on (:pounds))

