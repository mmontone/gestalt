;; This is tiny example of what we want to achieve, but it's not working at the moment. Its purpose is to make it work as a way of coming with a first version of the Gestalt framework.
(handler-bind ((SB-FASL::INVALID-FASL-VERSION (lambda (c) (declare (ignore
						c)) (invoke-restart
						'asdf::try-recompiling))
						))
(progn
(require :hunchentoot) ; web-server
(require :cl-who) ; html template language
(require :cl-cont) ; continuations
(require :anaphora) ; anaphoric macros
(require :alexandria) ; utilities
(require :contextl) ; context-oriented programming
(require :log5) ; system logging
(require :cl-ppcre) ; parsing and validation
(require :cl-interpol) ; parsing
(require :split-sequence) ; parsing
(require :closer-mop) ; mop
(require :rucksack) ; database
(require :xmlisp)   ; templates syntax
(require :ironclad)  ; url encryption
(require :cl-base64)  ; url codification
(require :trivial-garbage) ; weak references
;(require :uri-template) ; uri syntax
;(require :df) ; dataflow

)
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
	:xml
	))

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

(defclass entry-point-class (contextl:singleton-class)
  ())

(defvar *applications* (make-root-folder) "The registered applications")

;; TODO: create entry points from syntax
;#ep"/home/example"

(defvar *application* nil "The current application")

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

(define-condition object-already-exists (serious-condition)
  ((target :initarg :target
	   :accessor target
	   :documentation "The object that already exists"))
  (:report (lambda (c s)
	     (format s "~A already exists" (target c))))
  (:documentation "This condition is raised when we want to replace an existing object"))

(defmacro existence-protect ((object &key (report '(lambda (s)
						     (format s "Continue and replace the object")))
				      (signal '(lambda (object) (error 'object-already-exists :target object))))
			      &body body)
  (with-unique-names (replace-function)
    (once-only (object)
      `(flet ((,replace-function ()
	       ,@body))
	(restart-case
	    (progn
	      (when ,object
		(funcall ,signal ,object))
	      (,replace-function))
	  (superceed ()
	    :report ,report
	    (funcall #',replace-function)))))))

(defun replace-hash (key hash-table object)
  (multiple-value-bind (read-object exists-p)
      (gethash key hash-table)
    (declare (ignore read-object))
    (existence-protect (exists-p)
      (setf (gethash key hash-table) object))))

(defmethod %register-application ((app application))
  (multiple-value-bind (registered-app exists-p)
      (gethash (name app) *applications*)
    (declare (ignore registered-app))
    (existence-protect (exists-p :report (lambda (s)
					    (format stream "Replace the registered application"))
				  :signal (lambda (obj)
					    (declare (ignore obj))
					    (error 'application-exists :application-name (name app))))
			(log5:log-for info "Application registered: ~A" app)
			(setf (gethash (name app) *applications*) app))))

(defmethod register-application ((app application) &key (if-exists nil if-exists-provided))
  (if if-exists-provided
      (ecase if-exists
	(:superceed
	 (handler-bind ((superceed (lambda (condition)
				    (declare (ignore condition))
				    (invoke-restart 'superceed))))
	  (%register-application app))))
      (%register-application app)))

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

(defmacro defaction (name args &body body)
  `(defun/cc ,name ,args
     ,@body))

(defmacro action (name)
  "This function is meant to reference named actions as callbacks. Note that the dynamic-environment is caught at referencing-time, not at definition time with defaction"
  `(lambda-action (&rest args)
	     (declare (ignorable args))
	     (funcall (function ,name))))

(defmacro recording-vars (vars &body body)
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

(defmacro with-transaction ((&rest args) &body body)
  `(rucksack:with-transaction ,args
       (recording-vars (rucksack::*transaction*)
	 ,@body)))

(defmacro with-active-layers (layers &body body)
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
  "Syntax for defining applications."
  (flet ((process-application-args (args)
	   "Build the start function"
	   (let ((start-function-body (getf args :start)))
	     (when start-function-body
	       (setf (getf args :start)
		     `(lambda (application)
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
				,start-function-body)))))))))
	   args))
    `(make-instance 'application ,@(append (list :name (string name)) (process-application-args args)))))

(defapplication app-admin ()
  :entry-point (make-instance 'application-entry-point
			      :name "admin")
  :start (call 'applications-admin)
  :description "This application lets is the Gestalt applications admin. You can browse, configurate and execute them from its frontend")

(defun initialize-rendering-list ()
  (setf (session-value 'rendering-list *session*)
	(trivial-garbage:make-weak-hash-table :weakness :key-and-value)))

(defun render-rendering-list ()
  (loop for component in (session-value 'rendering-list *session*)
       when (is-active component)
       do (render-component component)))

(defun render-on-request (component)
  (let
      ((rendering-list
	(session-value 'rendering-list *session*)))
    (setf (gethash component rendering-list) component)))

(defclass applications-admin ()
  ((entry-points :component t
		 :initform (make-instance 'entry-points-list)
		 :reader entry-points))
  (:metaclass component-class))

(defclass model-component ()
  ((model :initarg model
	  :reader model
	  :initform (error "Provide the component model"))
   (:documentation "A mixin for components that hold a single model")))

(defclass list-component (model-component)
  ()
  (:metaclass component-class)
  (:documentation "A list simply renders its items on the screen. You can use it with either a cons list of a collection. In the first case, the component is registered to be updated on each request (as we don't have an events mechanism). You should consider using collections for better control"))

(defmethod initialize-instance :after ((list-component list-component) &rest initargs)
  (declare (ignore initargs))
  (render-list-elements list-component)
  (when (listp (model list-component))
    ; Register the component to be rendered on each request
    (render-on-request list-component)))

(defclass list-navigator (model-component)
  ((size :dataflow t
	 :initform 10
	 :documentation "The list size")
   (page :dataflow t
	 :initform 1
	 :documentation "The page to be displayed")
   (size-entry :component t)
   (page-entry :component t)
   (first :component t)
   (last :component t)
   (next :component t)
   (previous :component t)
   (list :component t))
  (:metaclass component-class))

(defmethod initialize-instance :after ((list-navigator list-navigator) &rest initargs)
  (with-df-slots (page size) list-navigator
    (with-object (list-navigator)
       (add-component 'size-entry
		   (make-instance 'number-entry :model size)) ; note that there shall be a conversion from number, to number value-holder
       (add-component 'page-entry
		   (make-instance 'number-entry :model page))
       (add-component 'first
		      (make-instance 'action-link
				     :action (lambda ()
					       (setf (page list-navigator) 1))))
       (add-component 'last
		      (make-instance 'action-link
				     :action (lambda ()
					       (setf (page list-navigator)
						     (multiple-value-bind (pages remainder)
							 (truncate (/ (length (model list-navigator))
								      (size list-navigator)))
							     (when (> remainder 0)
							       (incf pages))
							     (setf (page list-navigator) pages))))))
       (add-component 'next (make-instance 'action-link
					   :action (lambda ()
						     (incf (page list-navigator)))))
       (add-component 'previous (make-instance 'action-link
					       :action (lambda ()
							 (decf (page list-navigator)))))
       (add-component 'list (make-instance 'list-component
					   :model (extract-elements (model list-navigator)
								    :from (page list-navigator)
								    :by (size list-navigator)))))))

; a list-component simply displays every component that gets added to it
<template class="list-component">
  <list id="children"/>
</template>

<template class="list-navigator">
  <child id="list"/>
  <child id="size-entry"/>
  <child id="page-entry"/>
  <child id="first"/>
  <child id="last"/>
  <child id="next"/>
  <child id="previous"/>
</template>


(defclass list-navigator ()
  ())

(defmethod navigate-entry-point ((entry-point application-entry-point))


(defmethod initialize-instance :after ((admin applications-admin))
  (



(defapplication hola
    :entry-point (make-instance 'folder-entry-point :name "example/")
    :start (progn (print "hola")
		  (print "chau"))
    :description "This is an example application")

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
  ((allow-p :accessor allow-p
	    :initarg :allow-p
	    :initform (lambda () t)
	    :documentation "Specifies when the widget is allowed to be displayed"))
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

(defmacro lambda-action (args &body body)
  `(make-instance 'action
		  :continuation
		  (make-instance 'continuation :function (lambda/cc ,args ,@body))))

(defvar *session* nil "The current session")

(defun register-action (action &optional (session *session*))
  (dynamic-wind
    (let ((*actions* (cons action *actions*)))  ; The current control-flow transaction actions
      (proceed
       (setf (gethash (hash action) (actions session)) action)))))

(defun unregister-action (action &optional (session *session*))
  (remhash (hash action) (actions session)))

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

(defmacro atomically (&body body)
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

(defaction start-example-application (app)
  (call 'main-component))

(defcomponent main-component ()
  ((app-message :type label :text "This is an example application")))

(defaction initialize-main-component (comp)
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

;;; An acceptor that invokes the debugger on errors:
(defclass debuggable-acceptor (acceptor)
  ()
  (:documentation "This acceptor raises the SLIME debugger when an error occurs. Use as a mixin"))

(defmethod process-connection ((*acceptor* debuggable-acceptor) (socket t))
  (declare (ignore socket))
  (handler-bind ((error #'invoke-debugger))
    (call-next-method)))

(defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (handler-bind ((error #'invoke-debugger))
        (funcall dispatcher request)))))

(defclass gestalt-acceptor (acceptor debuggable-acceptor)
  ()
  (:documentation "The Gestalt Hunchentoot acceptor"))

;; (defclass session ()
;;   ((session-id :initarg :session-id
;; 	       :accessor session-id
;; 	       :documentation "The id under which the session is registered")
;;    (application :initarg :application
;; 		:accessor application
;; 		:initform *application*
;; 		:documentation "The application the session belongs to")
;;    (actions :initarg :actions
;; 	    :accessor actions
;; 	    :initform (make-hash-table :test #'equalp)
;; 	    :documentation "The actions")
;;    (root-component :initarg :root-component
;; 		   :accessor root-component
;; 		   :initform nil
;; 		   :documentation "The current component tree")
;;    (lock :accessor lock
;; 	 :initform (bordeaux-threads:make-lock "SESSION-LOCK")
;; 	 :documentation "The lock to be used for accessing the session from different threads"))
;;   (:documentation "An application session"))

;; (defmethod initialize-instance :after ((session session) &rest initargs)
;;   (%register-session session))


;; (defun %register-session (&optional session)
;;   (setf (session-id session) (generate-session-id (application session)))
;;   (add-session (application session) session))

;; (defun register-session ()
;;   (aif *session*
;;        it
;;        (make-instance 'session)))

;; (defmethod add-session ((app application) session)
;;   (replace-hash (session-id session)
;; 		(sessions (application session))
;; 		session))


(defun cont-to-str (cont)
  (let ((*package* (find-package "KEYWORD"))
        (*print-circle* t))
    (format nil "~A?~{~A&~}"
            (cl-base64:usb8-array-to-base64-string
             (encrypt
              (compress
               (prin1-to-string cont)))
             :uri t)
            (let ((toplevel-bindings
                   (find-toplevel-bindings cont)))
              (mapcar (lambda (binding)
                        (format nil "~A=~A"
                                (url-encode (prin1-to-string (car binding)))
                                (url-encode (prin1-to-string (cdr binding)))))
               toplevel-bindings)))))


(defvar *session-locks* (make-hash-table :test #'eq
                                         #+sbcl :weakness #+sbcl :key)
  "Per-session locks to avoid having unrelated threads
  waiting.")
#-sbcl(warn "No GC mechanism for *SESSION-LOCKS* on your Lisp. ~
            Expect a tiny memory leak until fixed.")

(defun session-lock ()
  (unless (gethash *session* *session-locks*)
    (setf (gethash *session* *session-locks*)
          (bordeaux-threads:make-lock (format nil "session lock for session ~S" *session*))))
  (gethash *session* *session-locks*))


(defmacro root-component (&optional (session '*session))
  "Use (root-component) and (setf (root-component) component)"
  `(session-value 'root-component ,session))

(defvar *system-actions* (make-hash-table :test #'equalp) "The registered actions")



(defvar *catch-errors-p* t)

(defvar *request-timeout* 10
  "Seconds until we abort a request because it took too long.
  This prevents threads from hogging the CPU indefinitely.

  You can set this to NIL to disable timeouts (not recommended).")




(push (hunchentoot:create-prefix-dispatcher
       "/test/"
       (make-continuation-handler 'test ; begin with test if URL
                                  "/test/" ; == this
                                  ))
      hunchentoot:*dispatch-table*)

(defun make-continuation-handler (default
                                  defaultp
                                  &optional (on-error
                                             (lambda (error)
                                               (format t "error:~%~A~%" error)
                                               (force-output)
                                               (cl-who:with-html-output-to-string (*standard-output*)
                                                 (:html (:head (:title "Oups"))
                                                        (:body "Someone messed up."))))))
  (let ((defaultp (if (or (symbolp defaultp)
                          (functionp defaultp))
                      defaultp
                      (lambda (name)
                        (string= name defaultp)))))
    (lambda ()
      (let ((b64-cont (first (last (split-sequence #\/
                                                   (script-name *request*)
                                                   :remove-empty-subseqs t))))
            (params   (mapcar (lambda (pair)
                                (let ((*read-eval* nil))
                                  (cons (read-from-string (car pair) nil)
                                        (read-from-string (cdr pair) nil))))
                              (reverse (get-parameters *request*)))))
        (multiple-value-bind (out error)
            (ignore-errors
              (if (and b64-cont
                       (not (funcall defaultp (script-name *request*))))
                  (let ((cont (read-from-string
                               (inflate
                                (decrypt
                                 (base64-string-to-usb8-array b64-cont
                                                              :uri t)))
                               nil)))
                    (setup-capture
                     (lambda ()
                       (invoke-cont (replace-bindings cont params)
                                    :value params))))
                  (setup-capture default)))
          (if (stringp out)
              out
              (funcall on-error error)))))))

(define-condition invalid-request ()
  ())

(define-condition invalid-session (invalid-request)
  ((url-value :initarg :url-value
	      :reader url-value
	      :documentation "The session url value"))
  (:report (c s)
	   (format s "The session ~S is invalid" (url-value c)))
  (:documentation "The session passed in the url is invalid"))

(define-condition invalid-action (invalid-request)
  ((url-value :initarg :url-value
	      :reader url-value
	      :documentation "The action url-value"))
    (:report (c s)
	     (format s "The action ~S is invalid" (url-value c)))
    (:documentation "The action passed in the url is invalid"))

(defun request-handler ()
  (flet ((handle-error (error)
	   (format t "Invalid session:~%~A~%" c)
	   (force-output)
	   (with-html-output-to-string (*standard-output*)
	     (:html (:head (:title "Oups"))
		    (:body "Someone messed up.")))))
    (handler-case
      (let* ((*session* (session-with-id (get-parameter "_s")))
	     (action (action-with-id (get-parameter "_c"))))
	;(let ((*application* (session-application *session*)))
	  (funcall action))
    (invalid-request (error)
      (handle-error error)))))

(defun action-url (action &optional (session *session*))
  (let ((application-url (application session)))
    (concatenate 'string application-url
		 "?_s=" (session-url-value session)
		 "&_c="	(action-url-value action))))

(defun encode-url-value (value)
  (cl-base64:usb8-array-to-base64-string
   (encrypt
    (sb-ext:string-to-octets
     (prin1-to-string value)))))

(defun session-url-value (&optional (session *session*))
  (encode-url-value (hash session)))

(defmethod hash (object)
  (sb-kernel:get-lisp-obj-address object))

(defun action-url-value (action)
  (encode-url-value (hash action)))

(defun decode-url-value (url-value)
  (sb-ext:octets-to-string
   (decrypt
    (cl-base64:base64-string-to-usb8-array url-value :uri t))))

(defun url-session (url-value)
  (destructuring-bind (session found-p)
      (gethash (parse-integer (decode-url-value url-value)) *sessions*)
    (when (not found-p)
      (error 'invalid-session :url-value url-value))
    session))

(defun url-action (url-value)
  (destructuring-bind (action found-p)
      (gethash (parse-integer (decode-url-value url-value)) *system-actions*)
    (when (not found-p)
      (error 'invalid-action :url-value url-value))
    action))

(hunchentoot:start (make-instance 'gestalt-acceptor :port 8080))