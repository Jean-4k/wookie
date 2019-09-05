(annot:enable-annot-syntax)
(in-package :wookie)



(defun clear-hooks (&optional hook)
  "Clear all hooks (default) or optionally a specific hook type."
  (vom:debug1 "(hook) Clearing ~a" (if hook
                                      (format nil "hook ~s" hook)
                                      "all hooks"))
  (if hook
      (setf (gethash hook (wookie-state-hooks *state*)) nil)
      (setf (wookie-state-hooks *state*) (make-hash-table :size 10 :test #'eq))))



(defun run-hooks (hook &rest args)
  "Run all hooks of a specific type. Returns a future that is finished with no
   values when all hooks have successfully run. If a hook callback returns a
   future object, then run-hooks will wait for it to finish before finishing its
   own future. If multiple callbacks return futures, run-hooks waits for ALL of
   them to finish before finishing its future.
   
   This setup allows an application to add extra processing to hooks that may be
   asynchronous without blocking the event loop, and without the processing of
   the current request stampeding full steam ahead when it may need access to
   information the hook is grabbing async.
   
   For instance, let's say you want to check user auth on each request, you
   could set up a :pre-route hook that reads the request and checks the auth
   info against your database, finishing the future it returns only when the
   database has responded. Once the future is finished, then Wookie will
   continue processing the request."
  (vom:debug1 "(hook) Run ~s" hook)
  (with-promise (resolve reject)
    (let ((hooks (gethash hook (wookie-state-hooks *state*)))
          (collected-promises nil)   ; holds futures returned from hook functions
          (last-hook nil))
      (block runner
        (handler-bind
            (((or error simple-error)
              (lambda (e)
                (unless wookie-config:*debug-on-error*
                  (let* ((hook-name (getf last-hook :name))
                         (hook-type hook)
                         (hook-id-str (format nil "~s" hook-type))
                         (hook-id-str (if hook-name
                                        (concatenate 'string hook-id-str (format nil " (~s)" hook-name))
                                        hook-id-str)))
                    (vom:error "(hook) Caught error while running hooks: ~a: ~a" hook-id-str e))
                  (reject e)
                  (return-from runner)))))
          (dolist (hook hooks)
            ;; track current hook for better error verbosity
            (setf last-hook hook)
            ;; see if a promise was returned from the hook function. if so, save it.
            (let ((ret (apply (getf hook :function) args)))
              (when (blackbird:promisep ret)
                (push ret collected-promises)))))
        (resolve (blackbird:all collected-promises))))))



(defmacro do-run-hooks ((socket) run-hook-cmd &body body)
  "Run a number of hooks, catch any errors while running said hooks, and if an
   error occurs, clear out all traces of the current request (specified on the
   socket). If no errors occur, run the body normally."
  (let ((sock (gensym "sock")))
    `(let ((,sock ,socket))
       (blackbird:catcher
         (blackbird:wait ,run-hook-cmd
           ,@body)
         (error (e)
           (let ((hook (when (eq (car ',run-hook-cmd) 'run-hooks)
                         (cadr ',run-hook-cmd))))
             (vom:error "(hook) Error running hooks: ~a: (socket ~a): ~a" hook ,socket e))
           (main-event-handler e ,socket)
           (if (as:socket-closed-p ,sock)
               ;; clear out the socket's data, just in case
               (setf (as:socket-data ,sock) nil)
               ;; reset the parser for this socket if it's open. this
               ;; should suffice as far as garbage collection goes.
               (setup-parser ,sock)))))))



@export
(defun add-hook (hook function &optional hook-name)
  "Add a hook into the wookie system. Hooks will be run in the order they were
   added."
  (vom:debug1 "(hook) Adding hook ~s ~a" hook (if hook-name
                                                 (format nil "(~s)" hook-name)
                                                 ""))
  ;; append instead of push since we want them to run in the order they were added
  (alexandria:appendf (gethash hook (wookie-state-hooks *state*))
                      (list (list :function function :name hook-name))))



(defun remove-hook (hook function/hook-name)
  "Remove a hook from a set of hooks by its function reference OR by the hook's
   name given at add-hook."
  (when (and function/hook-name
             (gethash hook (wookie-state-hooks *state*)))
    (vom:debug1 "(hook) Remove hook ~s" hook)
    (let ((new-hooks (remove-if
                       (lambda (hook)
                         (let ((fn (getf hook :function))
                               (name (getf hook :name)))
                           (or (eq fn function/hook-name)
                               (eq name function/hook-name))))
                       (gethash hook (wookie-state-hooks *state*)))))
      (setf (gethash hook (wookie-state-hooks *state*)) new-hooks))))
