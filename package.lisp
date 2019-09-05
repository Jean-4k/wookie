(defpackage :wookie
  (:use #:cl
	#:wookie-config
	#:wookie-util
	#:blackbird
	#:annot.class)
  (:shadow blackbird:*debug-on-error*)
  (:export #:get-header
           #:set-header
           
           #:response-error-response))



(in-package :wookie)




