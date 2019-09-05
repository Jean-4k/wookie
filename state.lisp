;;; This file defines and instantiates Wookie's global state handler. This is
;;; used throughout Wookie to store hook functions, routes, plugin state, etc.
;;; The having all state in one variable makes it a lot easier to thread Wookie
;;; without worrying about threads bumping into each other.



(annot:enable-annot-syntax)
(in-package :wookie)



@export-class
(defclass wookie-state ()
  ((hooks
    :initarg :hooks
    :initform (make-hash-table :size 10
			       :test #'eq)
    :accessor wookie-state-hooks
    :documentation "Holds the hook callbacks associated with this context.")
   (plugins
    :initarg :plugins
    :initform (make-hash-table :test #'eq)
    :accessor wookie-state-plugins
    :documentation "Holds the loaded plugins and their associated data for this context")
   (plugin-config
    :initarg :plugin-config
    :initform nil
    :accessor wookie-state-plugin-config
    :documentation "Holds all plugin configuration.")
   (routes
    :initarg :routes
    :initform (make-array 0
			  :adjustable t
			  :fill-pointer t)
    :accessor wookie-state-routes
    :documentation "Holds the routes this context uses.")
   (ordered-routes
    :initform nil
    :accessor wookie-state-ordered-routes
    :documentation "Routes ordered according to their priority (cached value)"))
  (:documentation
   "wookie-state holds all global data/state used by Wookie. It's purpose is to
     make threading Wookie easier by allowing the declaration of one
     thread-local variable instad of many."))



(defun make-wookie-state (&key hooks)
  "Create a wookie-state-object."
  (make-instance 'wookie-state
		 :hooks hooks))



@export
(defvar *state* (make-wookie-state)
  "Holds all global state/context for Wookie.")
