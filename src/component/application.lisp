(in-package :gestalt)

(defparameter *application* nil)
(defparameter *component* nil)
(defparameter *http-stream* nil)
(defparameter *session* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    `(with-html-output (*http-stream*)
       (htm
	,@body))))

(defclass application ()
  ((name :initarg :name
	 :initform (error "Provide the application name")
	 :accessor name)
   (title :initarg :title
	  :initform nil
	  :accessor title)
   (root :accessor root
	 :initarg :root
	 :initform (error "Provide the root component"))
   (stylesheets :initarg :stylesheets
		:accessor stylesheets
		:initform nil)
   (scripts :initarg :scripts
	    :accessor scripts
	    :initform nil)
   (static-directory
    :initarg :static-directory
    :accessor static-directory
    :initform nil)))

(defmethod title ((application application))
  (or (slot-value application 'title)
      (string (name application)))) 

(defun static (static-resource-path)
  (format nil "/static/~A" static-resource-path))

(defun include-stylesheet (stylesheet-path)
  (with-html
    (:link :rel "stylesheet" :href (princ-to-string stylesheet-path) :type "text/css")))

(defun include-script (script-path)
  (with-html
    (:script :type "text/javascript"
	     :language "javascript"
	     :src (princ-to-string script-path))))

(defmethod render ((application application))
  (let ((*application* application))
    (with-output-to-string (*http-stream*)
      (with-html
	(:html
	 (:head
	  (:title (title application))
	  (:meta :http-equiv "Content-Type"
		 :content "text/html; charset=UTF-8")
	  (:meta :http-equiv "Content-Style-Type" :content "text/css")
	  (:link :rel "shortcut icon"
		 :href (static "icons/favicon.ico"))
	  (loop for stylesheet in (stylesheets application)
	       do
	       (include-stylesheet stylesheet)))
	 (:body
	  (render (root application))
	  (loop for script in (scripts application)
	       do (include-script script))))))))

(defun unserialize-application-from-uri (uri)
  "Unserialize application"
  (let ((root (unserialize-component-from-uri uri (path 'root))))
    (validate-application (make-instance (find-argument (path 'app) uri) :root root))))

(defmethod serialize-to-uri ((application application) path)
  "Serializes the current application state"
  (let ((root-component (root application)))
    (append
     (serialize-to-uri root-component (path 'root))
     (list (cons (path 'app) (type-of application))))))

;; Logging

#+nil(start-sender 'info-messages  
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
