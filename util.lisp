(annot:enable-annot-syntax)



(defpackage :wookie-util
  (:use :cl :wookie-config :blackbird)
  (:shadow blackbird:*debug-on-error*)
  (:export #:querystringp
           #:set-querystring-hash
           #:querystring-to-hash
           #:body-to-string
           #:getf-reverse
           #:lookup-status-text))



(in-package :wookie-util)



(defun getf* (plist key &optional default)
  "Similar to GETF but compares keys with string-equal."
  (alexandria:doplist (k v plist default)
    (when (string-equal key k)
      (return-from getf* v))))



@export
(defmacro get-header (header-collection key)
  "Get a value from a header collection."
  (alexandria:with-gensyms (headers s-key)
    `(let ((,headers ,header-collection)
           (,s-key ,key))
       (cond ((hash-table-p ,headers)
              (let ((key (if (stringp ,s-key)
                             ,s-key
                             (string-downcase (string ,s-key)))))
                (gethash key ,headers)))
             ((listp ,headers)
              (let ((key (if (keywordp ,s-key)
                             ,s-key
                             (intern (string-upcase (string ,s-key)) :keyword))))
                (getf* ,headers key)))))))



@export
(defmacro set-header (header-collection key value)
  "Set a value into a header collection."
  (alexandria:with-gensyms (s-key)
    ;; note, we don't bind the collection here because that will destroy our getf
    ;; reference
    `(let ((,s-key ,key))
       (cond ((hash-table-p ,header-collection)
              (let ((key (if (stringp ,s-key)
                             ,s-key
                             (string-downcase (string ,s-key)))))
                (setf (gethash key ,header-collection) ,value)))
             ((listp ,header-collection)
              (let ((key (if (keywordp ,s-key)
                             ,s-key
                             (intern (string-upcase (string ,s-key)) :keyword))))
                (setf (getf ,header-collection key) ,value)))))))



@export
(defun map-plist (plist fn)
  "Iterate over a plist"
  (let ((result nil))
    (dotimes (i (/ (length plist) 2))
      (let (header value)
        (setf header (car plist)
              plist (cdr plist)
              value (car plist)
              plist (cdr plist))
        (let ((res (funcall fn header value)))
          (when res (push res result)))))
    (reverse result)))



@export
(defun camel-case (keyword)
  "Camel case anything that can be converted to a string (string, keyword,
   symbol, etc)."
  (let* ((lower (string-downcase (string keyword))))
    (loop for i from 0
          for c across lower do
      (let ((char-before (when (< 0 i)
                           (aref lower (1- i)))))
        (when (or (= i 0)
                  (eq char-before #\-)
                  (eq char-before #\space)
                  (eq char-before #\tab))
          (setf (aref lower i) (aref (string-upcase (string c)) 0)))))
    lower))



(defparameter *scanner-querystring-p*
  (cl-ppcre:create-scanner "^([a-z0-9-_\\[\\]]+(=([^&]+)?)?(&+|$))+" :case-insensitive-mode t)
  "Detects a querystring.")



(defun querystringp (querystring)
  "Detects if the given string is an HTTP querystring."
  (cl-ppcre:scan *scanner-querystring-p* querystring))



(defun get-querystring-hash-r (container subkeys val)
  "Recursively build a tree of hashes based on a given set of subkeys in a query
   string."
  (unless subkeys
    (return-from get-querystring-hash-r val))
  (let* ((key (car subkeys)))
    (let ((container (if container
                         container
                         (make-hash-table :test #'equal))))
      (let ((newcontainer (gethash key container)))
        (setf (gethash key container) (get-querystring-hash-r newcontainer (cdr subkeys) val)))
      container)))



(defun set-querystring-hash (hash key val)
  "Set the key of a querystring var into a hash, creating as many nested hashes
   as needed. For instance:

   (set-querystring-hash myhash \"data[body][tags]\" \"dogs,animals,bark\")

   Would update `myhash` with
     [body] => < hash: [tags] => \"dogs,animals,bark\" >"
  (let* ((subkeys (cl-ppcre:split "(\\]\\[|\\[|\\])" key))
         (subkeys (remove-if #'null subkeys)))
    (get-querystring-hash-r hash subkeys val)))



(defun convert-hash-vectors (hash)
  "Given a hash table, look for all hashes whos keys are a set of indexes
   starting at zero and convert them to vectors. For instance:
     {users: {0: 'leonard', 1: 'freddy', 2: 'samuel'}}
   becomes:
     {users: ['leonard', 'freddy', 'samuel']}

   This function is recursive."
  (unless (hash-table-p hash)
    (return-from convert-hash-vectors hash))
  (let* ((kv (loop for k being the hash-keys of hash
                   for v being the hash-values of hash  collect (cons k v)))
         (kv-length (length kv))
         (keys-num (remove nil (mapcar (lambda (kv) (ignore-errors (parse-integer (car kv))))
                                       kv)))
         (sum (reduce #'+ (mapcar (lambda (x) (1+ x)) keys-num))))
    (if (and (= (length keys-num) kv-length)
             (= sum (/ (* kv-length (1+ kv-length)) 2)))
        ;; we have a vector!
        (let ((vec (make-array kv-length)))
          (loop for (nil . v) in kv
                for idx in keys-num do
            (setf (aref vec idx) (convert-hash-vectors v)))
          vec)
        ;; same old boring hash
        (progn
          (loop for k being the hash-keys of hash
                for v being the hash-values of hash do
            (setf (gethash k hash) (convert-hash-vectors v)))
          hash))))



(defun querystring-to-hash (querystring)
  "Convert a querystring into a hash table."
  (if querystring
      (alexandria:alist-hash-table (quri:url-decode-params querystring) :test #'equal)
      (make-hash-table :test #'equal)))

(defun print-hash (hash-table &optional (indent 0))
  "Useful for debugging hash tables."
  (loop for k being the hash-keys of hash-table
        for v being the hash-values of hash-table
        do
    (dotimes (i indent) (format t " "))
    (if (hash-table-p v)
        (progn
          (format t "[~a] => ~%" k)
          (print-hash v (+ 2 indent)))
    (format t "[~a] => ~s~%" k v))))



(defun body-to-string (body-bytes content-type-header)
  "Given a byte vector of HTTP body data and the value of a Content-Type header,
   convert the body to a string via the charset provided in the header. If a
   character encoding is not provided, go with the HTTP default, ISO-8859-1."
  (let* ((charset-pos (search "charset=" content-type-header))
         (charset (if charset-pos
                      (intern (string-upcase (subseq content-type-header
                                                     (+ charset-pos 8))) :keyword)
                      :iso-8859-1)))
    (handler-case
      (babel:octets-to-string body-bytes :encoding charset)
      (error ()
        (babel:octets-to-string body-bytes :encoding :iso-8859-1)))))



(defun getf-reverse (plist key)
  "Like getf, except the VALUE comes before the KEY:
     '(:value1 :key1 :value2 :key2)
   Allows reverse lookups in plists without duplicating structures."
  (dotimes (i (length plist))
    (when (eq key (cadr plist))
      (return-from getf-reverse (car plist)))
    (setf plist (cddr plist))))



(defun lookup-status-text (status-code)
  "Get the HTTP standard text that goes along with a status code."
  (case status-code
    (100 "Continue")
    (101 "Switching Protocols")
    (102 "Processing")

    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoratative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (207 "Multi-Status")
    (208 "Already Reported")
    (226 "IM Used")

    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (306 "Switch Proxy")
    (307 "Temporary Redirect")
    (308 "Permanent Redirect")

    (400 "Bad Request")
    (401 "Unauthorized")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Request Entity Too Large")
    (414 "Request-URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Requested Range Not Satisfiable")
    (417 "Expectation Failed")
    (418 "I'm a teapot")
    (420 "Enhance Your Calm")
    (422 "Unprocessable Entity")
    (423 "Locked")
    (424 "Failed Dependency")
    (425 "Unordered Collection")
    (426 "Upgrade Required")
    (428 "Precondition Required")
    (429 "Too Many Requests")
    (431 "Request Header Fields Too Large")
    (444 "No Response")
    (449 "Retry With")
    (450 "Blocked by Windows Parental Controls")
    (451 "Unavailable For Legal Reasons")
    (494 "Request Header Too Large")
    (495 "Cert Error")
    (496 "No Cert")
    (497 "HTTP to HTTPS")
    (499 "Client Closed Request")

    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")
    (506 "Variant Also Negotiates")
    (507 "Insufficient Storage")
    (508 "Loop Detected")
    (509 "Bandwidth Limit Exceeded")
    (510 "Not Extended")
    (511 "Network Authentication Required")
    (598 "Network Read Timeout Error")
    (599 "Network Connect Timeout Error")
    (t "Unknown Status")))
