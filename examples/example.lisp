;; This is tiny example of what we want to achieve, but it's not working at the moment. Its purpose is to make it work as a way of coming with a first version of the Gestalt framework.
(progn
(require :hunchentoot)
(require :cl-who)
(require :cl-cont)
(require :anaphora)
(require :alexandria)
(require :contextl)
(require :log5)
(require :cl-ppcre)
(require :cl-interpol)
(require :split-sequence)
(require :closer-mop)
(require :rucksack)
)


(defpackage :gst-sketch
  (:use :cl
	:hunchentoot
	:cl-who
	:cl-cont
	:anaphora
	:alexandria
	:contextl
	:cl-ppcre
	:cl-interpol
	:split-sequence
	:closer-mop
	)
  )

(in-package :gst-sketch)

(log5:defcategory status)
(log5:defcategory info)

(log5:start-sender 'info  
  (log5:stream-sender :location *error-output*)  
  :category-spec '(info)
  :output-spec '(log5:message))

(defclass entry-point ()
  ((parent :initarg :parent
	   :accessor parent
	   :initform *applications*
	   :documentation "The entry-point folder")
   (name :initarg :name
	 :accessor name
	 :initform (error "Provide the entry-point name"))))

(defclass application-entry-point(entry-point)
  ((application :initarg :application
		:accessor application
		:documentation "The application at this entry-point"))
  (:documentation "An entry point with an application to run"))

(defclass folder-entry-point (entry-point)
  ((contents :initarg :contents
	     :accessor contents
	     :initform nil
	     :documentation "The folder contents"))
  (:documentation "An entry points folder"))

(defun make-root-folder ()
  (let ((root
	 (allocate-instance (find-class 'folder-entry-point))))
    (setf (slot-value root 'name) "/")
    (setf (slot-value root 'parent) nil)
    root))

(defmethod initialize-instance :after ((ep application-entry-point) &rest initargs)
  (declare (ignore initargs))
  ; Check the entry-point name
  (when (not (ppcre:all-matches-as-strings "^\\w+$" (name ep)))
    (error "The entry point name ~S is invalid" (name ep))))

(defmethod initialize-instance :after ((ep folder-entry-point) &rest initargs)
  (declare (ignore initargs))
  (let ((ep-name (name ep)))
    (when (not (ppcre:all-matches-as-strings "^\\w+\/$" (name ep)))
      (setf ep-name (concatenate 'string ep-name "/")))
    (when (not (ppcre:all-matches-as-strings "^\\w+\/$" ep-name))
      (error "The folder entry point name ~S is invalid" ep-name))
    (setf (name ep) ep-name)))


(defmethod ep-path ((ep entry-point))
  (concatenate 'string (ep-path (parent ep)) (name ep)))

(defmethod ep-path ((ep (eql nil)))
  "")

(defmethod print-object ((ep entry-point) stream)
  (print-unreadable-object (ep stream :type t :identity t)
    (format stream "~S" (ep-path ep))))

(defun parse-entry-point (string-path)
  (when (not (ppcre:all-matches "^(.*?/|.*?\\\\)?([^\\./|^\\.\\\\]+)(?:\\.([^\\\\]*)|)$" string-path))
    (error "~S is not a valid entry-point path" string-path))
  (loop for path in (split-sequence #\/ string-path)
       (make-entry-point 'folder-entry-point :name path)))

(defun entry-points ()
  *applications*)

(defmethod make-instance :around ((class entry-point-class) &rest initargs)
  (let ((ep-name (getf initargs :name)))
    (if (gethash ep-name *entry-points*)
  (error "The singleton class ~S cannot be instantiated." class)))))

(defvar *applications* (make-root-folder) "The registered applications")

;; TODO: create entry points from syntax
;#ep"/home/example"

(defclass application ()
  ((name :initarg :name :reader name
	 :initform (error "Provide the application name"))
   (entry-point :initarg :entry-point :reader entry-point
		:initform (error "Provide the application entry-point")
		:documentation "The entry point the application will be accesible")
   (description :initarg :description :reader description
		:initform (error "Provide the application description")
		:documentation "The application description (to be shown when applications are listed)")
   (start-function :initarg :start :reader start-function
		   :initform (error "Provide the start function")
		   :documentation "The function we run when the application starts")
   (root-component :accessor root-component
		   :documentation "The application root component"))
  (:documentation "A Gestalt Web application"))

(defmethod hash ((app application))
  (name app))

(defmethod initialize-instance :after ((app application) &rest initargs)
  (register-application app))

(defmethod print-object ((app application) stream)
  (print-unreadable-object (app stream :type t :identity t)
    (format stream "~A at: ~S" (name app) (entry-point app))))

(define-condition application-exists ()
  ((application-name :initarg :application-name
		     :reader application-name))
   (:report (lambda (condition stream)
	      (format stream "An application named ~S already exists" (application-name condition)))))

(defmethod %register-application ((app application))
  (flet ((register-app ()
	   (log5:log-for info "Application registered: ~A" app)  
	   (setf (gethash (name app) *applications*) app)))
    (restart-case
	(multiple-value-bind (registered-app exists-p)
	    (gethash (name app) *applications*)
	  (declare (ignore registered-app))
	  (if exists-p
	      (error 'application-exists :application-name (name app))
	      (register-app)))
      (superceed ()
	:report (lambda (stream)
		  (format stream "Replace the registered application"))
	(register-app)))))

(defmethod register-application ((app application) &key (if-exists nil if-exists-provided))
  (if if-exists-provided
      (ecase if-exists
	(:superceed
	 (handler-bind ((superceed (lambda (condition)
				    (declare (ignore condition))
				    (invoke-restart 'superceed))))
	  (%register-application app))))
      (%register-application app)))

(defvar *application* nil "The current application")

 
(defclass continuation ()
  ((function :initarg :function
	     :reader continuation-function
	     :initform (error "Provide the continuation function")
	     :documentation "The function to execute when this continuation is called")
   (dynamic-environment :initarg :dynamic-environment
			:reader dynamic-environment
			:initform (capture-dynamic-environment)
			:documentation "The dynamic-environment in which to call the continuation function"))
   (:metaclass funcallable-standard-class)
   (:documentation "A continuation. When a continuation is called the function is evaluated under the continuation dynamic environment"))

(defmethod initialize-instance :after ((cont continuation) &rest initargs)
  (declare (ignore initargs))
  (set-funcallable-instance-function cont (lambda (value)
					    (let ((env (dynamic-environment cont)))
					      (with-dynamic-environment (env)
						(funcall (continuation-function cont) value))))))

(defmacro defaction (name args &rest body)
  `(defun/cc ,name ,args
     ,@body))

(defmacro action (name)
  "This function is meant to reference named actions as callbacks. Note that the dynamic-environment is caught at referencing-time, not at definition time with defaction"
  `(lambda-action (&rest args)
	     (declare (ignorable args))
	     (funcall (function ,name))))

(defmacro recording-vars (vars &rest body)
  "Records dynamically bound variables in the dynamic-environment"
  (with-unique-names (proceed)
    (let ((gensyms (mapcar (lambda (var)
			     (declare (ignore var))
			     (gensym)) vars)))
      `(let
	   ,(loop for var in vars
	       for gensym in gensyms
	       collect `(,gensym ,var))
	 (dynamic-wind ,proceed
		       (let
			   ,(loop for var in vars
			       for gensym in gensyms
			       collect `(,var ,gensym))
			 (,proceed ,@body)))))))

(defmacro with-transaction ((&rest args) &rest body)
  `(rucksack:with-transaction ,args
       (recording-vars (rucksack::*transaction*)
		       ,@body)))

(defmacro with-active-layers (layers &rest body)
  `(with-active-layers ,layers
       (recording-vars (contextl::*active-context*)
		    ,@body)))

(defmacro dynamic-handler-case (expr cases)
  (with-gensyms (handler-case-proceed)
    `(dynamic-wind ,handler-case-proceed
		   (handler-case
		       (,handler-case-proceed ,expr)
		     ,cases))))

(defmacro defapplication (name &rest args)
  (flet ((process-application-args (args)
	   "Build the start function"
	   (let ((start-function-body (getf args :start)))
	     (when start-function-body
	       (setf (getf args :start)
		     `(lambda (application)
			(let ((env-holder (make-instance 'component)))
			  (flet ((component ()
				   env-holder))
			    (dynamic-wind
			     (let ((*application* application))
			       (proceed
				(macrolet ((call (component &rest initargs)
					     (declare (ignorable initargs))
					     (with-unique-names (comp)
					       (once-only (component)
						 `(let ((,comp (if (symbolp ,component)
							       (make-instance ',component ,@initargs)
							       ,component)))
						(setf (root-component application) ,component))))))
			      (with-call/cc
				,start-function-body)))))))))))
	   args))
    `(make-instance 'application ,@(append (list :name (string name)) (process-application-args args)))))

(defapplication hola
    :entry-point (make-instance 'folder-entry-point :name "example/")
    :start (progn (print "hola")
		  (print "chau"))
    :description "This is an example application")

(defapplication example-application ()
  ()
  (:entry-point "/example/")
  (:documentation "This application should let you edit a person's data and display a message-dialog whenever changes"))

(defclass component ()
  ((parent-holder :accessor parent-holder
		  :documentation "The component's parent parent-holder object")
   (children :accessor children
	     :initform (make-hash-table :test #'equalp)
	     :documentation "The component's children. It is a hash table that maps child slots to component-holder objects"))
  (:documentation "An application component"))

(defmethod add-child-component ((parent component) slot (child component))
  (let ((holder (make-instance 'component-holder :parent parent
			                         :child-slot slot
						 :child child)))
    (setf (parent-holder child) holder)
    (setf (gethash slot (children parent)) holder)))

(defun remove-child-component-at (parent-component slot)
  (remhash slot (children parent-component)))

(defmethod remove-child-component (child)
  (remhash (child-slot (parent-holder child)) (children (parent child))))

(defmethod parent ((component component))
  (parent (component-holder component)))

(defmethod (setf parent) ((component component) parent)
  (setf (parent (component-holder component)) parent))
				   
(defclass component-holder ()
  ((parent :initarg :parent
	   :accessor parent
	   :documentation "The parent component")
   (child-slot :initarg :child-slot
	       :accessor child-slot
	       :documentation "The child slot")
   (child :initarg :child
	  :accessor child
	  :documentation "The child component"))
  (:documentation "A component-holder is a parent component and child component indirection. It acts like a reference. We need it to modify the component tree properly"))

(defclass widget ()
  ()
  (:documentation "A widget"))

(defclass value-widget ()
  ((value :accessor value
	  :initarg :value
	  :documentation "The widget's value"))
  (:documentation "A widget that contains a value. This is meant to be used as a mixin"))

(defclass text-entry (widget value-widget)
  ()
  (:documentation "A widget that contains text"))

(defclass text-area (widget value-widget)
  ()
  (:documentation "A widget that contains a long text"))

(defclass label (widget value-widget)
  ()
  (:documentation "A widget that contains a text. It is not editable"))

(defclass callback-widget ()
  ((callback :initarg :callback
	     :accessor callback
	     :documentation "The widget's callback"))
  (:documentation "A widget that holds a callback. This is meant to be used as a mixin"))

(defclass button (widget callback-widget)
  ()
  (:documentation "A button"))

(defclass link (widget callback-widget)
  ()
  (:documentation "A link"))

(defclass navigation-link (link)
  ()
  (:documentation "A navigation link. These are treated specially for the URLs"))

(defclass action-link (link)
  ()
  (:documentation "An action link"))

(defclass widgets-renderer ()
  ()
  (:documentation "Renders a widget depending on the GUI type"))

(defclass html-widget-renderer (widget-renderer)
  ()
  (:documentation "An HTML widgets renderer"))

(defmethod render-widget ((renderer html-widget-renderer) (widget text-entry))
  (with-html-output (response *response*)
    (:input :type "text" :value (value widget))))

(defclass xul-widgets-renderer (widget-renderer)
  ()
  (:documentation "A XUL widgets renderer"))

(defclass gui-type ()

  (:documentation "The kind of GUI we have"))

(defclass html-gui-type ()
  ()
  (:documentation "An HTML GUI type"))

(defclass xul-gui-type ()
  ()
  (:documentation "A XUL GUI type"))

(defmacro defcomponent (name superclasses slots &rest initargs)
  (flet ((process-component-initargs (initargs)
	   (loop with code
	      for arg in initargs
	      do (case (car arg)
		   (:initialize (destructuring-bind (args &rest body) (cdr arg)
			      (push `(defmethod initialize-component ,(cons (list (car args) name) (cdr args))
				       (macrolet ((call (component &rest initgargs)
						    (declare (ignore component initargs))
						    (error "You cannot call a component from here. This is initialization call. Put your control flow code in the start method"))
						  (answer (&rest values)
						    (declare (ignore component initargs))
						    (error "You cannot answer from this component from here. This is initialization. Put your control flow code in the start method")))
				       ,@body))
				    code)
			      (remove arg initargs)))
		   (:start (destructuring-bind (args &rest body) (cdr arg)
			     (push `(defmethod start-component ,(cons (list (car args) name) (cdr args))
				      ,@(process-start-method-body body))
				   code)
			     (remove arg initargs)))))
	   (values initargs code)))
    (multiple-value-bind (class-initargs code) (process-component-initargs initargs)
      `(progn
	 (defclass ,name (cons 'component superclasses)
	   ,slots
	   ,@class-initargs)
	 ,@code))))

(defun process-start-method-body (self body)
  `(macrolet ((call (component &rest initargs)
		(declare (ignorable initargs))
		(with-unique-names (comp)
		  (once-only (component)
		    `(let ((,comp (if (symbolp ,component)
				      (make-instance ',component ,@initargs)
				      ,component)))
		       (call-component ,self ,component)))))
	      (answer (&rest values)
		`(return-from-component ,self ,@values)))
     (with-call/cc
       ,@start-function-body)))

(defclass action()
  ((continuation :initarg :continuation
		 :accessor continuation
		 :documentation "The action continuation")
   (component-tree :initarg :component-tree
		   :accessor component-tree
		   :documentation "The component-tree this action works on")
   (url :accessor url
	:documentation "The encoded URL"))
  (:metaclass funcallable-standard-class)
  (:documentation "This is a framework action"))

(defmacro lambda-action (args &rest body)
  `(make-instance 'action
		  :continuation
		  (make-instance 'continuation :function (lambda/cc ,args ,@body))))
  
(defvar *system-actions* (make-hash-table) "The currently active application actions")

(defun register-action (action)
  (setf (gethash (hash action) *system-actions*) action))

(defun unregister-action (action)
  (remhash (hash action) *system-actions*))

(defmethod initialize-instance :after ((action action) &rest initargs)
  (set-funcallable-instance-function action (lambda (value)
					      (funcall (continuation action) value)))
  (register-action action))

(defmethod print-object ((action action) stream)
  (print-unreadable-object (action stream :type t :identity t)
    (format stream "~S" (url action))))

(defun/cc call-component (caller callee)
  (call/cc (cont)
	   ;; Set up the continuation  
	   (setf (continuation callee)
		 (make-instance 'continuation
				:function cont))
	   ;; Replace the component in the tree
	   (setf (parent-holder caller) callee)
	   ;; Invoke the start function
	   (funcall (start-function callee))))

(defun/cc return-from-component (callee value)
  (funcall (continuation callee) value))

(defmacro atomically (&rest body)
  "Execute an application control flow transactionally"
  (with-unique-names (atomically-proceed)
    `(dynamic-wind ,atomically-proceed
		   (let ((*actions* nil))
		     (,atomically-proceed
		      ,@body)
		     ;; Remove the actions once the body is executed
		     (loop for action in *actions*
			  (unregister-action action))))))

(defclass url ()
  ((location :initarg :location
	     :accessor location 
	     :initform (error "Provide the location")
	     :documentation "The URL location")
   (resource-type :initarg :resource-type
		  :accessor resource-type
		  :initform (error "Provide the resource type")
		  :documentation "The kind of resource")
   (description :initarg :description
		:accessor description
		:initform "This URL has no description"
		:documentation "A short URL description"))
  (:documentation "An URL"))

(defvar *style-sheets* nil "The registered style-sheets")

(defclass style-sheet (url)
  ()
  (:documentation "An style sheet"))

(defun register-style-sheet (ss)
  (push ss *style-sheets*))

(defmethod initialize-instance :after ((ss style-sheet) &rest initargs)
  (setf (resource-type ss) :css)
  (register-style-sheet ss))
	   
(defvar *system-libraries* '() "The libraries available in the system")

(defclass library ()
  ((name :initarg :name
	 :initform (error "Provide the library name")
	 :documentation "The library name")
   (resources :initarg :resources
	      :initform '()
	      :documentation "The library resources")
   (description :initarg :description
		:accessor description
		:initform "This library has no description"
		:documentation "A short library description")
   (dependents :initarg :dependents
	       :accessor dependents
	       :initform '()
	       :documentation "The other libraries this library depends on"))
  (:documentation "A framework's library"))
   
(make-instance 'url
    :location "/example-style.css"
    :resource-type 'css)

(defun register-library (library)
  (push library *system-libraries*))

(defmacro deflibrary (name &rest initargs)
  `(make-instance 'library ,@(append `(:name ,(string name)) initargs)))

(defmethod initialize-instance :after ((lib library) &rest initargs)
  (register-library lib))

(deflibrary prototype-library
    :resources (list (make-instance 'url
				    :location "/prototype.js"
				    :resource-type 'js))
    :description "The Prototype javascript library")

(deflibrary scriptaculous-library
    :resources (list (make-instance 'url
				    :location "/scriptaculous.js"
				    :resource-type 'js))
    :dependents '(prototype-library)
    :description "The Scriptaculous javascript library")
 

;; TODO: provide syntax for URLs (like we should do for entry-points). Example: #\#url"http:/localhost/my-lib.css"

(defaction start ((app example-application))
  (call 'main-component))

(defcomponent main-component ()
  ((app-message :type label :text "This is an example application")))

(defaction initialize ((comp main-component))
  (let ((person (make-instance 'person)))
    (add-child (comp)
	(loop while t       
	     do (progn
		  (call (make-instance 'person-component :on person))
		  (call 'message-dialog :text (print-string person)))))))

(defcomponent person-component (model-component) ; model-component's hold models
  ((name :type text-input :on (name model))
   (lastname :type text-input :on (lastname model))
   (accept :type button :do (answer model))))

(deftemplate person-component (:style-sheets '(my-style-sheet))
  (with-xml-syntax
      <container id="name"/>
      <container id="lastname"/>
      <container id="accept"/>))