;;; This file holds the parser. It's the piece that ties *everything* together,
;;; from dispatching routes, body chunking, running a lot of essential hooks,
;;; etc.
;;;
;;; The parser is the heart of Wookie and holds most of the core logic. Most of
;;; the other pieces are icing on the cake.



(in-package :wookie)



(defun get-overridden-method (request original-method)
  "Checks if there is a GET var called _method, and if so, uses it instead of
   the provided method."
  (let* ((hash (plugin-request-data :get request))
         (val (when (hash-table-p hash)
                (gethash "_method" hash))))
    ;; if we passed in _method *and* we're not doing an OPTIONS call, replace
    ;; the HTTP method with the override
    (if (and (stringp val)
             (not (eq original-method :options)))
        (let ((new-method (intern (string-upcase val) :keyword)))
          ;; make sure it's copacetic
          (if (find new-method '(:get :post :delete :put :head :options :trace :connect))
              new-method
              original-method))
        original-method)))



(defun dispatch-route (parser route-dispatched sock request response route)
  "Dispatch the route under `route`. This not only handles calling
   the route's main function, but also handles use-next-route
   conditions and setting up intricate logic for resilient body
   chunking. It also runs the :pre-route and :post-route hooks."
  ;; dispatch the current route, but only if we haven't already done so
  (when route-dispatched
    (return-from dispatch-route))
  (setf route-dispatched
	t)
  (do-run-hooks (sock) (run-hooks :pre-route
				  request
				  response)
    (block skip-route
      (flet ((run-route (route)
               (if route
                   (let ((route-fn (getf route :curried-route)))
                     (vom:debug1 "(route) Dispatch ~a: ~s" sock route)
                     (funcall route-fn request response)
                     ;; if route expects chunking and all body chunks
                     ;; have come in already, run the chunk callback
                     ;; with the body buffer (otherwise the route's
                     ;; body callback will never be called).
                     (let ((request-body-cb (request-body-callback request)))
                       (when (and (getf route :allow-chunking)
                                  (getf route :buffer-body))
                         (if (and (body-buffer parser)
                                  (body-finished-p parser))
                             ;; the body has finished processing, either
                             ;; send it into the chunking function or set
                             ;; up a callback that is called when
                             ;; with-chunking is called that pumps the body
                             ;; into the newly setup chunking callback
                             (let ((body (fast-io:finish-output-buffer
					  (body-buffer parser))))
                               (if request-body-cb
                                   ;; with-chunking already called, great. pass
                                   ;; in the body
                                   (progn
                                     (funcall request-body-cb body t)
                                     (setf (body-buffer parser) nil))
                                   ;; set up a callback that fires when
                                   ;; with-chunking is called. it'll pass the
                                   ;; body into the with-chunking callback
                                   ;; once set
                                   (setf (request-body-callback-setcb request)
					 (lambda (body-cb)
					   (funcall body-cb body t)))))
                             ;; chunking hasn't started yet AND we haven't called
                             ;; with-chunking yet. this is kind of an edge case,
                             ;; but it needs to be handled. we set up a callback
                             ;; that runs once with-chunking is called that
                             ;; *hopes* the body has finished chunking and if so,
                             ;; fires with-chunking with the body.
                             (setf (request-body-callback-setcb request)
				   (lambda (body-cb)
                                     (when (body-buffer parser)
                                       (let ((body (fast-io:finish-output-buffer (body-buffer parser))))
                                         (funcall body-cb body (body-finished-p parser))
                                         (setf (body-buffer parser) nil)))))))))
                   (progn
                     (vom:warn "(route) Missing route: ~a ~s"
			       (request-method (request parser))
			       (route-path parser))
                     (funcall 'main-event-handler
			      (make-instance 'route-not-found
                                             :resource (route-path parser)
                                             :socket sock)
			      sock)
                     (return-from skip-route)))))
        ;; load our route, but if we encounter a use-next-route condition,
        ;; add the route to the exclude list and load the next route with
        ;; the same matching criteria as before
        (let ((route-exclude nil))
          (block run-route
            (loop
               (block next
                 (handler-bind
                     ((use-next-route
                       ;; caught a use-next-route condition, push the current
                       ;; route onto the exclude list, load the next route, and
                       ;; try again
                       (lambda (e)
                         (declare (ignore e))
                         (vom:debug1 "(route) Next route")
                         (push route
			       route-exclude)
                         (setf route
			       (find-route (fast-http:http-method http)
                                           (route-path parser)
                                           :exclude route-exclude))
                         (return-from next))))
                   ;; run our route and break the loop if successful
                   (progn
                     (run-route route)
                     (return-from run-route))))))))
      (do-run-hooks (sock) (run-hooks :post-route
				      request
				      response)
	nil))))



(defclass wookie-parser ()
  ((http :initarg :http
	 :initform (fast-http:make-http-request)
	 :accessor http)
   (route-path :initarg :route-path
	       :initform nil
	       :accessor route-path)
   (route :initarg :route
	  :initform nil
	  :accessor route
	  :documentation "Holds the current route, filled in below once we get headers.")
   (route-dispatched :initarg :route-dispatched
		     :initform nil
		     :accessor route-dispatched)
   (error-occurred-p :initarg :error-occurred-p
		    :initform nil
		    :accessor error-occurred-p)
   (request :initarg :request
	    :initform nil
	    :accessor request)
   (response :initarg :response
	     :initform nil
	     :accessor response)
   (request-body-buffer :initarg :request-body-buffer
			:initform nil
			:accessor request-body-buffer)
   (body-buffer :initarg :body-buffer
		:initform (fast-io:make-output-buffer)
		:accessor body-buffer)
   (body-finished-p :initarg :body-finished-p
		    :initform nil
		    :accessor body-finished-p)
   (body-callback-function :initarg :body-callback-function
			   :initform nil
			   :accessor body-callback-function)))



(defun make-wookie-parser (sock)
  (let* ((http (fast-http:make-http-request))
	 (request (make-instance 'request
				 :socket sock
				 :http http))
	 (response (make-instance 'response
				  :headers (make-hash-table :test #'equal)
				  :request request)))
    (make-instance 'wookie-parser
		   :request request
		   :response response)))



(defmethod request-logs ((parser wookie-parser)
			 headers)
  (let ((http (http parser))
	(host (get-header headers "host")))
    (vom:debug "(request) ~a ~a ~s ~a ~a"
	       (request parser)
	       (response parser)
	       (fast-http:http-method http)
	       (fast-http:http-resource http)
	       (if host
		   (concatenate 'string "(" host ")")
		   ""))))



(defmethod header-callback ((parser wookie-parser)
			    sock)
  "Called when our HTTP parser graciously passes us a block of
   parsed headers. Allows us to find which route we're going to
   dispatch to, and if needed, set up chunking *before* the body
   starts flowing in. Responsible for the :parsed-headers hook."
  (lambda (headers)
    (block header-callback-closure
      (blackbird:catcher
       (let* ((method (fast-http:http-method (http parser)))
              (resource (fast-http:http-resource (http parser)))
              (parsed-uri (quri:uri resource))
              (path (do-urlencode:urldecode (quri:uri-path parsed-uri)))
              (host (get-header headers "host")))
	 (request-logs parser headers)
	 (setf (route-path parser)
	       path)
	 ;; save the parsed uri for plugins/later code
	 (setf (request-uri (request parser)) parsed-uri
               (request-headers (request parser)) headers)
	 (do-run-hooks (sock) (run-hooks :parsed-headers
					 (request parser))
           ;; set up some tracking/state values now that we have headers
           ;; ALSO, check for _method var when routing.
           (let* ((method (get-overridden-method (request parser)
						 method))
                  (found-route (find-route method
					   path
					   :host host)))
             (setf route found-route
                   (request-method (request parser)) method
                   (request-resource (request parser)) resource)
             ;; handle "Expect: 100-continue" properly
             (when (string= (get-header headers "expect") "100-continue")
               (if found-route
                   (unless (getf route :suppress-100)
                     (send-100-continue (response parser)))
                   (progn
                     (funcall 'main-event-handler
			      (make-instance 'route-not-found
					     :resource (route-path parser)
					     :socket sock)
                              sock)
                     (return-from header-callback-closure))))
             ;; if we found a route and the route allows chunking, call the
             ;; route now so it can set up its chunk handler before we start
             ;; streaming the body chunks to it
             ;;
             ;; NOTE that we don't *need* to test if the data is actually
             ;; chunked for a chunk-enabled route to be able to receive the
             ;; data. if a chunk-enabled route gets called for data that
             ;; isn't chunked, it will receive all the data for that request
             ;; as one big chunk.
             (when (and found-route
			(getf found-route :allow-chunking))
               (dispatch-route parser
			       (route-dispatched parser)
			       sock
			       (request parser)
			       (response parser)
			       route)))))
       ;; pipe all uncaught errors we get to the main event handler
       ;; (with our socket object).
       ;;
       ;; note that errors that are caught here are more or less fatal
       ;; and as such, we stop processing the request any further by
       ;; setting `error-occurred-p` to T which notifies the other
       ;; callbacks (body/finished) that they shall not proceed any
       ;; further with this request.
       (error (e)
              (setf (error-occurred-p parser)
		    t)
              (main-event-handler e sock))))))



(defun store-body-in-request (chunk start end parser)
  (when (request-store-body (request parser))
    (unless (request-body-buffer parser)
      (setf (request-body-buffer parser)
	    (fast-io:make-output-buffer)))
    (if (< (+ (fast-io:buffer-position (request-body-buffer parser))
	      (- (or end (length chunk))
		 (or start 0)))
	   *max-body-size*)
	(progn
	  (fast-io:fast-write-sequence chunk
				       (request-body-buffer parser)
				       (or start 0)
				       end)
	  t)
	(progn
	  (send-response (response parser)
			 :status 413
			 :body "request body too large")
	  (setf (error-occurred-p parser) t)
	  nil))))



;; we have a body chunking callback and the body has
;; been buffering. append the latest chunk to the
;; buffer and send the entire buffer into the body cb.
;; then, nil the buffer so we know we don't have to do
;; body buffering anymore
(defun body-chunking-callback-and-body-has-been-buffering (chunk start end parser)
  (fast-io:fast-write-sequence chunk
			       (body-buffer parser)
			       start
			       end)
  (funcall request-body-cb
	   (fast-io:finish-output-buffer (body-buffer parser))
	   (body-finished-p parser))
  (setf (body-buffer parser) nil))



;; we have a chunking callback set up by the route, no
;; need to do anything fancy. just send the chunk in.
(defun body-chunking-callback-set-by-route (chunk start end parser)
  (funcall request-body-cb
	   chunk
	   (body-finished-p parser)
	   :start start
	   :end end))


;; we're allowing chunking through this route, we're
;; allowing body buffering through this route, and the
;; chunking callback hasn't been set up yet (possible
;; if the client starts streaming the body before our
;; :pre-route hook(s) finish their futures). create a
;; body buffer if we don't have one and start saving
;; our chunks to it (until our route has a chance to
;; set up the chunking cb).
(defun body-chunking-callback-not-set (chunk start end parser)
  (fast-io:fast-write-sequence chunk
			       (body-buffer parser)
			       start
			       end))



(defun forward-chunk-to-callback-provided-in-chunk-enabled-router (chunk start end parser sock route)
  (do-run-hooks (sock) (run-hooks :body-chunk
				  (request parser)
				  chunk
				  start
				  end
				  (body-finished-p parser))
    (let ((request-body-cb (request-body-callback (request parser))))
      (cond ((and request-body-cb
		  (body-buffer parser))
	     (body-chunking-callback-and-body-has-been-buffering chunk
								 start
								 end
								 parser))
	    (request-body-cb
	     (body-chunking-callback-set-by-route chunk start end parser))
	    ((and (getf route :allow-chunking)
		  (getf route :buffer-body))
	     (body-chunking-callback-not-set chunk start end parser))))))



(defmethod body-callback ((parser wookie-parser)
			  sock)
  "Called (sometimes multiple times per request) when the HTTP
   parser sends us a chunk of body content, which mainly occurs
   during a chunked HTTP request. This function is responsible
   for calling our :body-chunk, and also for buffering body data
   if the route allows."
  (setf (body-callback-function parser)
	(lambda (chunk start end)
	  (block body-callback-closure
	    (when (error-occurred-p parser)
	      (return-from body-callback-closure))
	    ;; store the body in the request
	    (unless (store-body-in-request chunk start end parser)
	      (return-from body-callback-closure))
	    ;; forward the chunk to the callback provided in the chunk-enabled
	    ;; router
	    (forward-chunk-to-callback-provided-in-chunk-enabled-router chunk start end parser sock route)))))



(defmethod finish-callback ((parser wookie-parser)
			    sock)
  "Called when an entire HTTP request has been parsed. Responsible 
   for the :body-complete hook."
  (lambda ()
    (block finish-callback-closure
      (when (error-occurred-p parser)
	(return-from finish-callback-closure))
      (setf (body-finished-p parser)
	    t)
      ;; set the request body into the request object
      (when (and (request-body-buffer parser)
		 (request-store-body (request parser)))
	(setf (request-body (request parser))
	      (fast-io:finish-output-buffer (request-body-buffer parser)))
	(setf (request-body-buffer parser)
	      nil))   ; because i'm paranoid
      (funcall (body-callback-function parser)
	       (make-array 0
			   :element-type 'cl-async:octet)
	       0
	       0)
      ;; make sure we always dispatch at the end.
      (do-run-hooks (sock) (run-hooks :body-complete (request parser))
	(dispatch-route parser
			(route-dispatched parser)
			sock
			(request parser)
			(response parser)
			route)))))


(defun setup-parser (sock)
  "This is the main parser function. It's responsible for listening to a socket,
   setting up an HTTP parser for it, handling the different events/callbacks the
   HTTP parser throws at it, dispatching routes, handling body chunking, etc. A
   lot of this is done via shared state which all lives under this function's
   top-level (let) form, and a set of local functions which read/modify this
   shared state.

   This function is at the core of Wookie's being.

   Note that setup-parser can be called multiple times on the same socket. The
   only reason to do this is if a request/response has come and gone on the
   socket and you wish to make the socket available for another request. Wookie
   handles all of this automatically."
  (let ((parser (make-wookie-parser sock)))
    (setf (as:socket-data sock)
	  (list :request (request parser)
		:response (response parser)))
    ;; make an HTTP parser. will be attached to the socket and will be
    ;; responsible for running all of the above callbacks directly as data
    ;; filters in from the read callback.
    (let ((http-parser (fast-http:make-parser
			(http parser)
			:header-callback (header-callback parser sock)
			:body-callback (body-callback parser sock)
			:finish-callback (finish-callback parser sock))))
      ;; attach parser to socket-data so we can deref it in the read callback
      (setf (request-parser (request parser))
	    parser)
      (setf (getf (as:socket-data sock)
		  :parser)
	    http-parser))))



(defun handle-connection (sock)
  "Handles a new connection. Creates a bunch of closures that are passed into an
   http-parse parser which decide amongst themselves, during different points in
   the parsing, when to dispatch to the found router, when to send chunked
   content to the route, etc."
  ;; TODO pass client address info into :connect hook
  (do-run-hooks (sock) (run-hooks :connect sock)
    (setup-parser sock)))



(defun read-data (sock data)
  "A simple read-cb handler that passes data to the HTTP parser attached to the
   socket the data is coming in on. The parser runs all necessary callbacks
   directly, so this function just blindly feeds the data in."
  ;; grab the parser stored in the socket and pipe the data into it
  (let ((parser (getf (as:socket-data sock) :parser)))
    (handler-bind
        ((error (lambda (e)
                  (when (typep e 'fast-http.error:parsing-error)
                    (let* ((data (as:socket-data sock))
                           (response (getf data :response)))
                      (vom:error "error parsing: ~a" e)
                      (when response
                        (send-response response :status 400 :body "Error parsing client HTTP request")))))))
      (funcall parser data))))
