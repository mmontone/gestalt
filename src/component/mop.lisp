(in-package :gestalt)

(defclass standard-component-class (standard-class)
  ()
  (:documentation "Components metaclass"))

(defmethod shared-initialize :around ((class standard-component-class) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from standard-component"
  (let* ((standard-component-class (find-class 'standard-component-class))
	 (standard-component (find-class 'component))
	 (not-already-component (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) standard-component-class))))
    (if (and (not (eq class standard-component)) not-already-component)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list standard-component)) args)
	(call-next-method))))

(defmethod closer-mop:validate-superclass ((class standard-component-class)
					   (super standard-class))
  t)

(defclass component-slot-mixin ()
  ((component-slot-p
    :initarg :component
    :initform nil
    :accessor component-slot-p)
   (serialize-p :initarg :serialize
		:initform nil
		:accessor serialize-p)
   (serialization-name :initarg :serialization-name
		       :initform nil
		       :accessor serialization-name)))

(defmethod serialization-name ((slot component-slot-mixin))
  (aif (slot-value slot 'serialization-name)
       it
       (closer-mop:slot-definition-name slot)))      

(defmethod print-object ((slot-definition component-slot-mixin) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream "name: ~A component: ~A serialize: ~A (~A)"
	    (closer-mop:slot-definition-name slot-definition)
	    (component-slot-p slot-definition)
	    (serialize-p slot-definition)
	    (serialization-name slot-definition))))

(defclass component-direct-slot-definition
    (component-slot-mixin closer-mop:standard-direct-slot-definition)
  ())

(defclass component-effective-slot-definition
    (component-slot-mixin closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:slot-value-using-class ((class standard-component-class) object slot-definition)
  "When setting a component slot, a component is added as child instead, and the slot holds its key"
  (if (component-slot-p slot-definition)
      ;; If it is a component slot, retrieve the child component
      (get-component object (closer-mop:slot-definition-name slot-definition))
      ;; else, get the slot value
      (call-next-method)))
		 
(defmethod (setf closer-mop:slot-value-using-class) (new-value
						     (class standard-component-class)
						     object
						     slot-definition)
  (if (component-slot-p slot-definition)
      (progn
	(check-type new-value component)
	;; If we are setting a component slot, then we just add the component
	(add-component object (closer-mop:slot-definition-name slot-definition) new-value))
      ;; else, normal assignment
      (call-next-method)))     

(defmethod closer-mop:direct-slot-definition-class ((class standard-component-class)
						    &rest initargs)
  (declare (ignore initargs))
  (find-class 'component-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class standard-component-class)
						       &rest initargs)
  (declare (ignore initargs))
  (find-class 'component-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class standard-component-class) name direct-slots)
  (let ((effective-slot (call-next-method)))
     (setf (component-slot-p effective-slot)
           (some #'component-slot-p direct-slots))
     (setf (serialize-p effective-slot)
	   (some #'serialize-p direct-slots))
     (setf (serialization-name effective-slot)
	   (some #'serialization-name direct-slots))
     effective-slot))

(defmethod component-slot-p ((object closer-mop:slot-definition))
  nil)

(defmethod serialize-p ((object closer-mop:slot-definition))
  nil)

(defmethod serialization-name ((object closer-mop:slot-definition))
  nil)

(defmethod serializable-slots ((class standard-component-class))
  (loop for slot in (closer-mop:class-slots class)
       when (serialize-p slot)
       collect slot))

(defmethod component-slots ((class standard-component-class))
  (loop for slot in (closer-mop:class-slots class)
       when (component-slot-p slot)
       collect slot))
