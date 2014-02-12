(in-package :gestalt)

(defclass component ()
  ((parent :initarg :parent
	   :accessor parent
	   :initform nil)
   (children :initarg :children
	     :accessor children
	     :initform (make-hash-table :test #'equalp))))

(defclass session ()
  ((values :initform (make-hash-table :test #'equalp)))
  (:documentation "Sessions are always serialized"))
	   

(defclass component-holder ()
  ((active-component :initarg :active-component
		     :accessor active-component
		     :initform nil)
   (call-chain :accessor call-chain
	       :initform nil
	       :documentation "The call chain. List with format (component . callback)")
   (serialize :initarg :serialize
	      :accessor serialize-p
	      :initform t)))

(defun get-component-key (component child)
  (loop for holder being the hash-values of (children component)
     using (hash-key key)
     when (eql (active-component holder) child)
     do (return key)))

;; Path handling
(defun path (&rest args)
  (alexandria:flatten (append (mapcar #'list args))))

(defun component-path (component)
  (aif (parent component)
       (path
	(component-path it)
	(get-component-key it component))
       (path 'root)))

(defun path-string (path)
  (format nil "~{~a~^.~}"
	  (mapcar (lambda (s)
		    (string-downcase (symbol-name s)))
		  path)))

(defun component-path-string (component)
  (path-string (component-path component)))

(defun path-from-string (string)
  (mapcar (lambda (s)
	    (intern (string-upcase s)))
	  (split-sequence:split-sequence #\. string)))

(defun prefixed-uri-arguments (prefix uri)
  (loop for (arg . value) in uri
     when (equalp (subseq arg 0 (length prefix)) prefix)
     collect (cons arg value)))

(defun find-argument (path uri)
  (loop for (arg . value) in uri
       when (equalp path arg)
       do (return-from find-argument value))
  nil)

(defun get-component-in-path (path &optional (application *application*))
  (let ((component (root application)))
    (loop for slot in (subseq path 1)
       do (setf component (get-component component slot)))
    ;(break "Component in path: ~A ~A" path component)
    component))

(defmacro defcomponent (name supers slots &rest options)
  (let (class-options component-options)
    (loop for option in options
       do
	 (cond
	   ((member (first option) (list :render :initialize))
	    (push option component-options))
	   (t (push option class-options))))
    `(progn
       (defclass ,name ,supers
	 ,slots
	 ,@class-options
	 (:metaclass standard-component-class))
       ,@(loop for option in component-options
	    collect
	      (case (first option)
		(:render (destructuring-bind (render (comp) &body body) option
			   (declare (ignore render))
			   `(define-renderer ((,comp ,name))
			      ,@body)))
		(:initialize (destructuring-bind (initialize (comp &optional initargs) &body body) option
			       (declare (ignore initialize))
			       `(defmethod initialize-instance :after ((,comp ,name) &rest ,(or initargs 'initargs))
					   (declare (ignorable ,(or initargs 'initargs)))
					   ,@body))))))))

(defgeneric render (component))

(defmethod render :around ((component component))
  (let ((*component* component))
    (with-html-output (*http-stream*)
      (htm (:div :id (component-path-string component)
		 (call-next-method))))))

(defmacro define-renderer (args &body body)
  `(defmethod render ,args
     (with-html-output (*http-stream*)
       ,@body)))

(defmacro define-serialization (component-type &body body)
  `(defmethod serialize-to-uri ((component ,component-type) path)
     ,@body))

(defmacro define-unserialization (component-type &body body)
  `(defmethod unserialize-from-uri (uri path (component ,component-type))
     ,@body))

(defun add-component (self slot component)
  (format t "Adding ~A to ~A in ~A~%" component self slot)
  (let ((comp (gethash slot (children component))))
    ;; If a component already exists in that slot, remove it
    (when comp
      (remove-component component slot))
    (setf (gethash slot (children self))
	  (make-instance 'component-holder :active-component component))
    (setf (parent component) self)))

(defun remove-component (self slot)
  (let ((component (get-component self slot)))
    (remhash slot (children component))
    (setf (parent component) nil)))

(defun get-component (component slot)
  (aif
   (gethash slot (children component))
   (active-component it)
   (error "Children component ~A not found in ~A" slot component)))

(defun child-components (component)
  (loop for holder being the hash-values of (children component)
       collect
       (active-component holder)))

(defun get-component-holder (component)
  (let ((parent (parent component)))
    (loop for component-holder being the hash-values of (children parent)
	 when (eql (active-component component-holder) component)
	 do (return-from get-component-holder component-holder)))
  (error "No component holder found for ~A" component))

(defun call (self component &optional answer-handler)
  (let ((component-holder (get-component-holder self)))
    (push (cons (active-component component-holder)
		answer-handler)
	  (call-chain component-holder))
    (setf (active-component component-holder) component)))

(defun answer (component result)
  (let ((component-holder (get-component-holder component)))
    (destructuring-bind (previous-component . answer-handler)
	(pop (call-chain component-holder))
      (setf (active-component component-holder) previous-component)
      (when answer-handler
	(funcall answer-handler previous-component result)))))

(defmethod serialize-to-uri ((component component) path)
  (append
   (list
    ;; Serialize the component first

    ;; Serialize the type
    (cons (path path 'type) (type-of component)))

   ;; Serialize the serializable slots
   (loop for slot in (serializable-slots (class-of component))
      collect
	(let ((slot-path (path path (serialization-name slot))))
	  (serialize-to-uri (slot-value component (closer-mop:slot-definition-name slot))
			    slot-path)))
   
    ;; Serialize its children
   (loop for component-holder being the hash-values of (children component)
      using (hash-key slot)
      appending
	(serialize-to-uri component-holder (path path slot)))))

(defmethod serialize-to-uri ((component-holder component-holder) path)
  "Serialize a component holder to uris"
  (if (null (call-chain component-holder))
      ;; There are no components being called. Ignore the component holder, just serialize the active-component
      (serialize-to-uri (active-component component-holder) path)
      ;; else, serialize the component holder and the call chain
      (append
       (list 
	(cons (path path 'type) 'ch)) ;; ch for component-holder
       (serialize-to-uri (active-component component-holder)
			 (path path 'a)	;; a for active-component
			 )
       (loop for (comp . answer-handler) in (call-chain component-holder)
	  for i = 1 then (1+ i)
	  appending
	    (let ((component-path (path path (intern (format nil "C~a" i))))
		  (answer-handler-path (path path (intern (format nil "AH~a" i)))))
	      (list (cons component-path
			  (serialize-to-uri comp component-path))
		    (cons answer-handler-path
			  answer-handler)))))))

(defmethod unserialize-from-uri (uri path (component component))
  (loop for slot in (serializable-slots (class-of component))
     do
       (let* ((slot-path (path path (serialization-name slot)))
	      (slot-value (aif (find-argument (path slot-path 'type) uri)
			       ;; We assume it is a class
			       (unserialize-from-uri uri slot-path (make-instance it))
			       ;; A primitive type?
			       (or (find-argument slot-path uri)
				   (error "Couldn't unserialize slot ~A from ~A" slot-path uri)))))
	 ;(break "Unserializing: ~A ~A to ~A" component (closer-mop:slot-definition-name slot) slot-value)
	 (setf (slot-value component (closer-mop:slot-definition-name slot))
	       slot-value)))
  (loop for key being the hash-keys of (children component)
       do
       (let ((holder
	      (unserialize-component-holder-from-uri uri (path path key))))
	 (setf (parent (active-component holder)) component)
	 (setf (gethash key (children component)) holder)))
  component)

(defun unserialize-component-from-uri (uri path)
  "Unserialize a component"
  (let ((component-type (find-argument (append path (list 'type)) uri)))
    (when (not component-type)
      (error "Cannot unserialize component from uri ~A" uri))
    (let ((component (make-instance component-type)))
      (unserialize-from-uri uri path component))))

(defun unserialize-component-holder-from-uri (uri path)
  (let ((args (prefixed-uri-arguments path uri)))
    (let ((component-holder (make-instance 'component-holder)))
      (aif (find-argument (path path 'ch) args)
	   ;; The component holder was serialized. Unserialize.
	   (let ((active-component
		  (unserialize-component-from-uri
		   uri
		   (path path 'a)))
		 (call-chain
		  ;; Serialize the call chain
		  (loop with call-chain = nil
		     for i = 1 then (1+ i)
		     do
		       (let* ((component-path (path path (intern (format nil "C~a" i))))
			      (component-args (prefixed-uri-arguments component-path uri)))
			 (when (null component-args)
			   (return call-chain))
			 (let ((component (unserialize-component-from-uri
					   component-args
					   component-path))
			       (answer-handler (find-argument path (intern (format nil "AH~a" i)))))
			   (push (cons component answer-handler) call-chain))))))
	     (setf (active-component component-holder)
		   active-component)
	     (setf (call-chain component-holder)
		   call-chain))
	   ;; else, unserliaze the the component directly
	   (let ((component
		  (unserialize-component-from-uri args path)))
	     (setf (active-component component-holder)
		   component)))
      component-holder)))

