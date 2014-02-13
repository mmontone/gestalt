(in-package :gestalt)

(defmacro define-action (name-and-options args &body body)
  (if (listp name-and-options)
      (destructuring-bind (name &key toplevel) name-and-options
	`(progn
	   (defun ,name ,args ,@body)
	   (setf (get ',name :toplevel) ,toplevel)
	   (setf (get ',name :action-p) t)))
      `(progn
	 (defun ,name-and-options ,args ,@body)
	 (setf (get ',name-and-options :action-p) t))))

(defmethod serialize-to-uri ((x string) path)
  (cons path x))

(defmethod serialize-to-uri ((x integer) path)
  (cons path x))

(defun action-link% (action component &rest args)
  (check-type action symbol)
  (when (not (get action :action-p))
    (error "~A is not an action" action))
  (let ((app-state
	 (or (get action :toplevel)
	     (serialize-to-uri *application* nil)))
	(action-args (append `(:_C ,(component-path component)) args)))
    ;(break "Action: serializing component path ~A ~A" *component* (component-path *component*))
    (format nil "/~A?_a=~A&_z=~A" action
	    (encode-string action-args)
	    (encode-string app-state))))

(defmacro action-link (action component &rest args)
  `(action-link% ',action ,component ,@args))

(defun unserialize-action (action args)
  ;(break "Unserializaing action component: ~A" (getf args :_C))
  (lambda ()
    (apply (symbol-function action)
	   (get-component-in-path (getf args :_C))
	   (alexandria:remove-from-plist args :_C))))
				    
