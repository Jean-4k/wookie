(defpackage :wookie
  (:use #:cl
	#:wookie-config
	#:wookie-util
	#:blackbird
	#:annot.class)
  (:shadow blackbird:*debug-on-error*)
  (:export #:get-header
           #:set-header
           
           #:response-error-response

           #:plugin-config
           #:plugin-request-data
           #:load-plugins
           #:unload-plugin
           #:defplugin
           #:defplugfun

           #:listener
           #:listener-bind
           #:listener-port
           #:listener-backlog
           #:start-server

           #-(or :wookie-no-ssl) #:ssl-listener
           #-(or :wookie-no-ssl) #:listener-certificate
           #-(or :wookie-no-ssl) #:listener-key
           #-(or :wookie-no-ssl) #:listener-password))
