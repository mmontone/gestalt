(in-package :gst.view.templates)

; Mop

(defclass standard-template-class (standard-class)
  ((template-combination :initarg :template-combination
			 :accessor template-combination
			 :initform (list 'standard-template-combination)))
  (:documentation "Standard metaclass for templates"))

(defclass standard-template-combination-slot ()
  ((component-class :initarg :component-class
		    :accessor component-class
		    :initform (error "Provide the component-class"))
   (primary-template :initarg :primary-template
		    :reader primary-template
		    :initform nil)
   (above-template :initarg :above-template
		    :reader above-template
		    :initform nil)
   (below-template :initarg :below-template
		   :reader below-template
		   :initform nil)
   (around-template :initarg :around-template
		    :reader around-template
		    :initform nil))
  (:documentation "Class used to store templates of the standard-template-combination"))

(defclass template-combination ()
  ()
  (:documentation
   "Superclass of all template combinations.
    Templates can be combined depending on the template-combination.
    Template combinations can be defined through a MOP"))

(defclass standard-template-combination (template-combination)
  ((combination-slots
    :reader combination-slots
    :initform (make-hash-table)
    :documentation "The defined templates by the component
                    class name they are defined upon"))
  (:documentation "The default template-combination"))

(defmethod validate-superclass ((class standard-template-class)
				(super standard-class))
  t)

(defmethod initialize-instance :after
    ((class standard-template-class) &rest initargs)
  (setf (template-combination class)
	(make-instance (find-class (first (template-combination class))))))

(defmethod print-object ((slot standard-template-combination-slot) stream)
  (print-unreadable-object (slot stream :type t :identity t)
    (format stream "on ~A" (component-class slot))))

(defmethod (setf primary-template) (template
				    (slot standard-template-combination-slot))
  (when (not (null (primary-template slot)))
    (warn "Redefining :primary template for ~A" (component-class template)))
  (setf (slot-value slot 'primary-template) template))

(defmethod (setf above-template) (template
				  (slot standard-template-combination-slot))
  (when (not (null (above-template slot)))
    (warn "Redefining :above template for ~A" (component-class template)))
  (setf (slot-value slot 'above-template) template))

(defmethod (setf below-template) (template
				  (slot standard-template-combination-slot))
  (when (not (null (below-template slot)))
    (warn "Redefining :below template for ~A" (component-class template)))
  (setf (slot-value slot 'below-template) template))

(defmethod (setf around-template) (template
				   (slot standard-template-combination-slot))
  (when (not (null (around-template slot)))
    (warn "Redefining :around template for ~A" (component-class template)))
  (setf (slot-value slot 'around-template) template))

(defmethod template-combination-register-template
    ((template-combination standard-template-combination) template)
  (multiple-value-bind (combination-slot found-p)
      (gethash (component-class template)
	       (combination-slots template-combination))
    (when (not found-p)
      (setf (gethash (component-class template)
		     (combination-slots template-combination))
	   (make-instance 'standard-template-combination-slot
			  :component-class (component-class template)))
      (setf combination-slot (gethash (component-class template)
				      (combination-slots template-combination))))
    (let ((qualifier (first (qualifiers template))))
      (cond
	((emptyp qualifier)
	 (setf (primary-template combination-slot) template))
	((string-equal qualifier "PRIMARY")
	 (setf (primary-template combination-slot) template))
	((string-equal qualifier "ABOVE")
	 (setf (above-template combination-slot) template))
	((string-equal qualifier "BELOW")
	 (setf (below-template combination-slot) template))
	((string-equal qualifier "AROUND")
	 (setf (around-template combination-slot) template))
	(t (error "Invalid qualifier ~A for standard-template-combination" qualifier))))))

(defvar *template-class-assignments*
  (make-hash-table))

(defun assign-component-template-class (component-class template-class)
  (setf (gethash component-class *template-class-assignments*) template-class))

(define-condition template-class-assignment-error (serious-condition)
  ())

(define-condition template-class-already-assigned-error (template-class-assignment-error)
  ((component-class :initarg :component-class
		    :reader component-class)
   (assigned-template-class :initarg :assigned-template-class
			    :reader assigned-template-class)
   (template-class :initarg :template-class
		   :reader template-class))
  (:report (lambda (c s)
	     (format s "~A is already assigned to ~A when trying to assign ~A to it"
		     (assigned-template-class c)
		     (component-class c)
		     (template-class c)))))

(define-condition template-class-not-assigned-error (template-class-assignment-error)
  ((component-class :initarg :component-class
		    :reader component-class))
  (:report (lambda (c s)
	     (format s "template-class not assigned to ~A"
		     (component-class c)))))

(defgeneric ensure-template-class-assignment (component-class template-class &key if-does-not-exist)
  (:documentation "Ensures that template-class has been assigned to component-class")
  (:method ((component-class symbol) template-class &key if-does-not-exist)
    (declare (ignore if-does-not-exist))
    (ensure-template-class-assignment (find-class component-class) template-class))
  (:method (component (template-class symbol) &key if-does-not-exist)
    (declare (ignore if-does-not-exist))
    (ensure-template-class-assignment component (find-class template-class)))
  (:method ((component-class standard-class)
	    (template-class standard-class)
	    &key (if-does-not-exist :assign))
    (multiple-value-bind (assigned-template-class found-p)
	(gethash component-class *template-class-assignments*)
      (if found-p
	  (when (not (eql template-class assigned-template-class))
	    ; error: the template-class assigment is different for
	    ; the component-class
	    (error 'template-class-already-assigned-error
	       :component-class component-class
	       :assigned-template-class assigned-template-class
	       :template-class template-class))
	(case if-does-not-exist
	  (:assign (progn
		     (assign-component-template-class component-class
						      template-class)
		     (warn "Implicitly assigning ~A to ~A"
			   template-class
			   component-class)))
	  (:error (restart-case
		      (error 'template-class-not-assigned-error
			     :component-class component-class)
		    (assign ()
		      :report (lambda (stream)
				(format stream "Assign ~A to ~A"
					template-class
					component-class))
		      (assign-component-template-class component-class
						       template-class))))
	  (t (error "if-does-not-exists parameter must be one of :assign, :error")))))))

(defmethod template-class-register-template
    ((class standard-template-class) template)
  (ensure-template-class-assignment (component-class template)
				    class)
  (template-combination-register-template (template-combination class)
				 template))

(defun register-template (template)
  (template-class-register-template (class-of template)
				    template))

(defun template-classes ()
  ; TODO: rewrite. The flatten invocation is a hack
  (labels ((all-subclasses (class)
	   (let ((all-subclasses (closer-mop:class-direct-subclasses class)))
	     (append all-subclasses
		     (loop for subclass in all-subclasses
			  collect (all-subclasses subclass))))))
    (let ((standard-template-class (find-class 'standard-template)))
      (flatten
       (cons standard-template-class
	     (all-subclasses standard-template-class))))))

(defmethod find-all-templates-for-class ((class symbol))
  (find-all-templates-for-class (find-class class)))

(defmethod find-all-templates-for-class ((class standard-object))
  (let ((template-classes (template-classes)))
    (loop for template-class in template-classes
       append (find-templates-for-class component template-class))))

(defmethod find-templates-for-class ((class symbol) template-class)
  (find-templates-for-class (find-class class) template-class))

(defmethod find-templates-for-class ((class standard-object)
				     (template-class standard-template-class))
  (template-combination-find-templates-for-class class (template-combination template-class)))

(defmethod template-combination-find-templates-for-class
    (class (template-combination standard-template-combination))
  (multiple-value-bind (combination-slot found-p)
      (gethash class (combination-slots template-combination))
    (when found-p
      (flatten (list (primary-template combination-slot)
		     (around-template combination-slot)
		     (above-template combination-slot)
		     (below-template combination-slot))))))

(defun templates-for-class (class)
  (loop for template in (template-classes)
       append (find-templates-for-class class template)))

;; Templates

(defclass standard-template (xml-container)
   ((component-class
     :initform nil
     :documentation "The class of components this template applies to")
    (component-id
     :initarg :component-id
     :accessor component-id
     :initform ""
     :documentation "The id of the child component this template affects")
    (description
     :initarg :description
     :accessor description
     :initform "This template has not description")
    (libraries
     :initarg :libraries
     :accessor libraries
     :initform nil
     :documentation "The libraries the template depends on")
    (style-sheets
     :initarg :style-sheets
     :accessor style-sheets
     :initform nil
     :documentation "The style-sheets the template depends on")
    (qualifiers :initarg :qualifiers
		:accessor qualifiers
		:initform ""
		:documentation "Qualifiers used by template-combinations, COP, etc")
    (local-templates :accessor local-templates
		     :initform (make-hash-table)
		     :documentation "Holds the locally defined templates"))
  (:metaclass standard-template-class)
  (:documentation "The standard Gestalt template"))

(defclass template (standard-template)
  ()
  (:metaclass standard-template-class)
  (:documentation "Nickname for standard-template"))

(defvar *template* '()
  "Variable to track the template that is being created")

(defun unregister-all-templates ()
  (loop for template-class in (template-classes)
       do (clear-templates
	   (template-combination template-class)))
  t)

(defmethod clear-templates ((template-combination
				      standard-template-combination))
  (clrhash (slot-value template-combination 'combination-slots)))

(defmethod initialize-instance :before ((template standard-template) &rest initargs)
  (declare (ignore initargs))
  (push template *template*))

(defmethod xml:finished-reading :after ((template standard-template) stream)
  (declare (ignore stream))
  (setf *template* (cdr *template*))
  ;; Set the correct variable values
  (setf (component-id template)
	(if (emptyp (component-id template))
	    nil
	    (intern (component-id template))))
  (setf (qualifiers template)
	(if (emptyp (qualifiers template))
	    '()
	    (split-sequence:split-sequence #\ (qualifiers template))))
  (if (null *template*)
      (register-template template)
      (let ((parent-template (first *template*)))
	(register-local-template parent-template template))))

(defmethod register-local-template ((template standard-template)
				    (local-template standard-template))
  (let ((component-class (component-class local-template)))
    (multiple-value-bind (registered-local-template found-p)
	(gethash component-class
		 (local-templates template))
      	(declare (ignore registered-local-template))
	(if found-p
	    (error "There's another local-template defined for class ~A in template ~A"
	       component-class
	       template)
	    (setf (gethash component-class (local-templates template))
		  local-template)))))

(defmethod component-class ((template standard-template))
  (find-class (read-from-string (slot-value template 'component-class))))

(defclass style-sheets (xml-container)
  ()
  (:documentation "Template style-sheets"))

(defclass style-sheet (xml-container)
  ((url :accessor url
	:initform ""))
  (:documentation "A stylesheet"))

(defclass libraries (xml-container)
  ()
  (:documentation "Template libraries"))

(defclass library (xml-node)
  ((name :accessor name
	 :initform ""))
  (:documentation "A library"))

(defclass component (xml-node)
  ((slot :accessor slot
	 :initform ""))
  (:documentation "Special tag. Embeds the component named by the id attribute"))

(defclass body (xml-container)
  ()
  (:documentation "Node holding templates body"))

(defclass components (xml-container)
  ((slot :accessor slot
	 :initform "")
   (var :accessor var
	:initform "component"
	:documentation "The iteration variable"))
  (:documentation "A slot that contains a collection of components"))

(defclass update (xml-node)
  ())

(defclass container (update)
  ((id :accessor id)))

(defclass *if (update xml-container)
  ((cond :accessor cond
	 :initform "")))

(defclass then (xml-container)
  ())

(defclass else (xml-container)
  ())

(defclass *loop (update xml-container)
  ((var :accessor var
	:initform "")
   (collection :accessor collection
	       :initform "")))

(defclass next-template (xml-node)
  ())

#|

(defclass person-viewer ()
  ())

(defclass person-editor ()
  ())

(swank:inspect-in-emacs
<template component-class="person-editor"
          description="A template for a person-editor"
	  qualifiers="primary">
   <libraries>
     <library name="prototype-library"/>
   </libraries>
   <style-sheets>
     <style-sheet url="http://www.my-style.com/style.css"/>
     <style-sheet url="http://www.my-style.com/style2.css"/>
   </style-sheets>
   <body>
     <div>
       <p>Hola!!</p>
       <a href="http://www.agentsheets.com">AgentSheets</a>
       <a href="http://www.agentsheets2.com">AgentSheets</a>
       <container id="name"/>
       <container id="lastname"/>
       <container id="address"/>
       <template component-class="person-viewer">
          <p>Chaoooo!!</p>
       </template>
     </div>
   </body>
</template>)

(swank:inspect-in-emacs
<template component-class="person-editor"
          description="A template for a person-editor"
	  qualifiers="around">
  <next-template/>
  <input type="button">Save</input>
</template>)

COP in templates can be introduced in templates by defining a new template class and a new template-combination:

(defclass cop-template-combination (standard-template-combination)
  ()
  (:documentation "Context Oriented Programming support for templates"))

(defclass cop-template (standard-template)
  ((layer :initarg :layer
	  :accessor layer
	  :initform ""
	  :documentation "The layer the template belongs to"))
  (:template-combination cop-template-combination)
  (:metaclass standard-template-class))

And then we can do:

(swank:inspect-in-emacs
<cop-template component-class="person-viewer"
          description="A template for a person-editor"
	  qualifiers="primary"
          layer="view-layer">
   <libraries>
     <library name="prototype-library"/>
   </libraries>
   <style-sheets>
     <style-sheet url="http://www.my-style.com/style.css"/>
     <style-sheet url="http://www.my-style.com/style2.css"/>
   </style-sheets>
   <body>
     <div>
       <p>Hola!!</p>
       <a href="http://www.agentsheets.com">AgentSheets</a>
       <a href="http://www.agentsheets2.com">AgentSheets</a>
       <container id="name"/>
       <container id="lastname"/>
       <container id="address"/>
       <template component-class="person-viewer">
          <p>Chaoooo!!</p>
       </template>
     </div>
   </body>
</cop-template>)

|#