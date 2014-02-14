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
  (let ((*application* application))
    (with-output-to-string (*http-stream*)
      (render (root application)))))

(defun unserialize-application-from-uri (uri)
  "Unserialize application"
  (let ((root (unserialize-component-from-uri uri (path 'root))))
    (validate-application (make-instance 'application :root root))))

(defmethod serialize-to-uri ((application application) path)
  "Serializes the current application state"
  (let ((root-component (root application)))
    (serialize-to-uri root-component (path 'root))))

;; Logging

(start-sender 'info-messages  
  (stream-sender :location *error-output*)  
  :category-spec '(info+)  
  :output-spec '(category message))

;; Debugging
(setf hunchentoot:*catch-errors-p* nil)

(defun validate-application (application)
  "Debugging: check application consistency"
  (log-for info "Validating application")
  (validate-component (root application))
  application)

(defun validate-component (component)
  (loop for holder being the hash-value of (children component)
       do
       (progn
	 (assert (typep holder 'component-holder))
	 (assert (eql (parent (active-component holder))
		      component))
	 (validate-component (active-component holder)))))
