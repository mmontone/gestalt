(in-package :gestalt)

(defparameter *application* nil)
(defparameter *component* nil)
(defparameter *http-stream* nil)
(defparameter *session* nil)

(defclass application ()
  ((root :accessor root
	 :initarg :root
	 :initform (error "Provide the root component"))))

(defmethod render ((application application))
  (with-output-to-string (*http-stream*)
    (render (root application))))

(defun unserialize-application-from-uri (uri)
  "Unserialize application"
  (let ((root (unserialize-component-from-uri uri (path 'root))))
    (make-instance 'application :root root)))

(defmethod serialize-to-uri ((application application) path)
  "Serializes the current application state"
  (let ((root-component (root application)))
    (serialize-to-uri root-component (path 'root))))
