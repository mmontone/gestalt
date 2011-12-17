(defvar *url-handlers* '() "URL handler functions")

(defun dispatch (url)
  (loop for handler in *url-handlers*
     do
       (multiple-value-bind (matchp response) (funcall handler url)
	 (when matchp
	   (return-from dispatch response))))
  (error 'url-dispatch-error :error "Could not dispatch the url ~s" url))

(defmacro defurl-handler (name args &body body)
  "URL handlers take an URL string and return two values. The first, t or nil, depending on if the url match the handler. The second, the server response."
  `(progn
     (defun ,name ,args ,@body)
     (push #',name *url-handlers*)))

(defurl-handler default-url-handler (url)
  "This is the default url handler. We suppose the url represents a continuation request. We try to build one and execute it"
  (handler-case
      (let ((request (build-request-from-url url)))
	(values t (execute request)))
    (build-request-error (e)
      (values nil nil))))

(defclass gst-request (hunchentoot:request)
  ())

(defvar *session* nil
  "The current session object")

(defvar *sessions* (make-hash-table :test #'equalp)
  "Session objects table")

(defclass gst-session ()
  ((continuations :accessor continuations :initform (make-hash-table :test #'equalp))))

(defun session-from-id (id)
  (multiple-value-bind (foundp session)
      (gethash id *sessions*)
    (when (not foundp)
      (error 'session-not-found-error (format nil "There's no session with id ~s" id)))
    session))

(defclass session-request (request)
  ((session-id :accessor session-id :initarg :session-id)))

(defclass resource-request (request)
  ((resource-id :accessor resource-id :initarg :resource-id)))

(defclass continuation-request (session-request)
  ((continuation-id :accessor continuation-id :initarg :continuation-id)))

(defmethod execute :around ((req session-request))
  (let ((*session* (session-from-id (session-id req))))
    (call-next-method)))

(defmethod execute ((req continuation-request))
  (eval-continuation (continuation-from-id (continuation-id req) *session*)))

(defun continuation-from-id (cont-id session)
  (multiple-value-bind (foundp cont) (gethash cont-id (continuations session))
    (when (not foundp)
      (error 'invalid-continuation-error (format nil "The continuation is invalid: ~s" cont-id)))))

(defun eval-continuation (cont)
  (funcall cont))

;; Hunchentoot plugging

