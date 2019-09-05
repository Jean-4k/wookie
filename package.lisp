(defpackage :wookie
  (:use #:cl
	#:wookie-config
	#:wookie-util
	#:blackbird
	#:annot.class)
  (:shadow blackbird:*debug-on-error*)
  (:export #:*state*
           
           #:get-header
           #:set-header
           
           #:remove-hook
           
           #:route-error
           #:route-not-found
           #:clear-routes
           #:clear-route
           #:next-route
           #:defroute
           #:with-vhost

           #:clear-hooks
           #:add-hook
           #:remove-hook

           #:response-error
           #:response-error-response
           #:response-already-sent
           #:request
           #:request-socket
           #:request-method
           #:request-resource
           #:request-headers
           #:request-uri
           #:request-data
           #:request-store-body
           #:request-body
           #:request-plugin-data
           #:request-body-callback
           #:request-http
           #:response
           #:response-request
           #:response-headers
           #:response-finished-p
           #:get-socket
           #:with-chunking
           #:send-response
           #:start-response
           #:finish-response
           #:send-100-continue

           #:*enabled-plugins*
           #:*plugin-folders*
           #:register-plugin
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
