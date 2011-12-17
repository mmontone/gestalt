(in-package :gestalt)

(defclass standard-component-class (standard-class)
  ()
  (:documentation "Components metaclass"))

(defmethod shared-initialize :around ((class standard-component-class) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from standard-component"
  (let* ((standard-component-class (find-class 'standard-component-class))
	 (standard-component (find-class 'standard-component))
	 (not-already-component (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) standard-component-class))))
    (if (and (not (eq class standard-component)) not-already-component)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list standard-component)) args)
	(call-next-method))))

(defmethod validate-superclass ((class standard-component-class)
				(super standard-class))
  t)

(defclass component-slot-mixin ()
  ((component-slot-p
    :initarg :component
    :initform nil
    :accessor component-slot-p)))

(defmethod print-object ((slot-definition component-slot-mixin) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream "name: ~A component: ~A"
	    (slot-definition-name slot-definition)
	    (component-slot-p slot-definition))))

(defclass component-direct-slot-definition
    (component-slot-mixin standard-direct-slot-definition)
  ())

(defclass component-effective-slot-definition
    (component-slot-mixin standard-effective-slot-definition)
  ())

(defmethod shared-initialize :after ((obj standard-component) slot-names &rest initargs)
  (declare (ignore initargs))
  (loop
     for slot in (class-slots (class-of obj))
     when (component-slot-p slot)
     do (let ((slot-symbol (read-from-string (slot-definition-name slot))))
	  (add-component obj
			 slot-symbol
			 (slot-value obj slot-symbol)))))

(defmethod (setf slot-value-using-class) (new-value
					  (class standard-component-class)
					  object
					  slot-definition)
  (remove-component object (slot-definition-name slot-definition))
  (add-component object (slot-definition-name) new-value)
  (call-next-method))

(defmethod direct-slot-definition-class ((class standard-component-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'component-direct-slot-definition))

(defmethod effective-slot-definition-class ((class standard-component-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'component-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class standard-component-class) name direct-slots)
   (let ((effective-slot (call-next-method)))
     (setf (component-slot-p effective-slot)
           (some #'component-slot-p direct-slots))
     effective-slot))

(defmethod component-slot-p ((object slot-definition))
  nil)