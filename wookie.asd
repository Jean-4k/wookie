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
  :components
  ((:file "config")
   (:file "util" :depends-on ("config"))
   (:file "package" :depends-on ("util"))
   (:file "state" :depends-on ("package"))
   (:file "error" :depends-on ("package" "state"))
   (:file "route" :depends-on ("error"))
   (:file "plugin" :depends-on ("package" "state"))
   (:file "hook" :depends-on ("package" "state"))
   (:file "request-response" :depends-on ("error" "hook"))
   (:file "parser" :depends-on ("error" "request-response" "route" "hook" "plugin"))
   (:file "listener" :depends-on ("error" "request-response" "route" "hook" "plugin" "parser"))
   #-(or :wookie-no-ssl) (:file "listener-ssl" :depends-on ("listener"))
   (:file "helper" :depends-on ("listener" "plugin" "package")))
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
