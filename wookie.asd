(asdf:defsystem :wookie
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.3.15"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:alexandria
               #:blackbird
               #:cl-async 
               #-(or :wookie-no-ssl) #:cl-async-ssl
               #:cl-ppcre
               #:babel
               #:chunga
               #:fast-http
               #:quri
               #:do-urlencode
               #:cl-fad
               #:fast-io
               #:vom
	       #:cl-annot)
  :serial t
  :components ((:file "config")
	       (:file "util")
	       (:file "package")
	       (:file "state")
	       (:file "error")
	       (:file "route")
	       (:file "plugin")
	       (:file "hook")
	       (:file "request-response")
	       (:file "parser")
	       (:file "listener")
	       #-(or :wookie-no-ssl) (:file "listener-ssl")
	       (:file "helper"))
  :in-order-to ((test-op (test-op :wookie/tests))))



(asdf:defsystem :wookie/tests
  :author "Jean-4k <45103062+Jean-4k@users.noreply.github.com>"
  :license "MIT"
  :version "0.0.0"
  :description "Tests for wookie."
  :defsystem-depends-on (#:prove)
  :depends-on (#:wookie
	       #:prove)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c))
  :pathname "tests/"
  :components ((:file "package")
	       (:file "wookie")))
