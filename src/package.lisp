;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:cliocp
  (:use #:cl #:cffi)
  (:export #:nexus
	   #:with-nexus
	   #:open-nexus
	   #:close-nexus
	   #:etypecase*
	   #:nexus-devices
	   #:nexus-requests

	   #:device
	   #:close-device
	   #:register-device
	   #:unregister-device
	   #:find-device
	   
	   #:request
	   #:request-id
	   #:schedule-request
	   #:get-completion-result 
	   #:release-request
	   
	   ;; sockets
	   #:connected-socket
	   #:open-connected-socket
	   #:connect-request
	   #:disconnect-request
	   #:send-request
	   #:recv-request
	   #:open-listening-socket
	   #:listening-socket
	   #:accept-request
	   #:accept-request-conn

	   ;; timers
	   #:timer
	   #:timer-request
	   #:cancel-timer
	   
	   ;; files
	   #:file
	   #:open-file
	   #:read-request
	   #:write-request
	   
	   ;; errors
	   #:completion-timeout
	   #:completion-interrupted
	   #:io-error
	   #:io-error-dev
	   #:io-error-req
	   ))



	   


