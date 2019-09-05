(annot:enable-annot-syntax)
(in-package :wookie)



@export-class
(defclass listener ()
  ((bind :accessor listener-bind :initarg :bind :initform nil)
   (port :accessor listener-port :initarg :port :initform 80)
   (backlog :accessor listener-backlog :initarg :backlog :initform -1)
   (event-cb :accessor listener-event-cb :initarg :event-cb :initform nil))
  (:documentation "Describes an HTTP listener."))



@export
(defgeneric start-server (listener)
  (:documentation
    "Start Wookie with the given listener."))



(defmethod start-server ((listener listener))
  ;; start the async server
  (vom:notice "(start) Starting Wookie  ~a:~a"
              (if (listener-bind listener)
                  (listener-bind listener)
                  "0.0.0.0")
              (listener-port listener))
  (as:tcp-server (listener-bind listener) (listener-port listener)
    'read-data
    :event-cb (lambda (ev) (listener-event-handler ev (listener-event-cb listener)))
    :connect-cb 'handle-connection
    :backlog (listener-backlog listener)))

