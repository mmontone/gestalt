(in-package :gstutils)

(defclass required-slots-object ()
  ()
  (:documentation "We need this super class just to be able to dispatch initialization methods for required slots checking"))

(defclass required-slots-class (standard-class)
  ((required-slots :initform '()
		   :accessor required-slots)))

(defclass required-slot-mixin ()
  ((required :initarg :required :initform nil :accessor required-slot-p)
   (error-msg :initarg :error-msg :accessor error-msg)))

(defmethod print-object ((slot-definition required-slot-mixin) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream "name: ~A required: ~A"
	    (slot-definition-name slot-definition)
	    (required-slot-p slot-definition))))

(defmethod initialize-instance ((slot-definition required-slot-mixin) &rest initargs)
  "We set the default :error-msg string"
  (call-next-method)
  (when (not (getf initargs :error-msg))
    (setf (error-msg slot-definition) (format nil "~A is required" (slot-definition-name slot-definition)))))

(defclass required-direct-slot-definition (required-slot-mixin standard-direct-slot-definition)
  ())

(defclass required-effective-slot-definition (required-slot-mixin standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class required-slots-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'required-direct-slot-definition))

(defmethod effective-slot-definition-class ((class required-slots-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'required-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class required-slots-class) name direct-slots)
  (let ((effective-slot (call-next-method)))
    (setf (required-slot-p effective-slot)
	  (some #'required-slot-p direct-slots))
    (setf (error-msg effective-slot)
	  (loop for direct-slot in direct-slots
	       when (error-msg direct-slot)
	       return (error-msg direct-slot)))
    effective-slot))

(defmethod required-slot-p ((slot-definition slot-definition))
  nil)

(defmethod error-msg ((slot-definition slot-definition))
  nil)

;; The above code is not tested at all!!
;; Now, I would like to be able to combine several metaclasses somehow.
;; example: (defclass getalt-model-class (required-slots-class persistent-object-class))
;; But I don't think that will work so easily!!

;; required-slots-class test

(defmethod validate-superclass ((class required-slots-class) (superclass standard-class))
  t)

(defmethod shared-initialize :around ((class required-slots-class) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from require-slots-object"
  (let* ((required-slots-metaclass (find-class 'required-slots-class))
	 (required-slots-class (find-class 'required-slots-object))
	 (not-already-required-slots
	  (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) required-slots-metaclass))))
    (if (and (not (eq class required-slots-class)) not-already-required-slots)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list required-slots-class)) args)
	(call-next-method))))

(defun make-keyword (name)
  (values (intern (string-upcase name) "KEYWORD")))

(define-condition required-slot-error (serious-condition)
  ())

(defmethod shared-initialize :after ((obj required-slots-object) slot-names &rest initargs)
  "Initialize the dataflow slots of the object"
  (loop for slot-definition in (class-slots (class-of obj))
	do
       (when (and (required-slot-p slot-definition)
		  (not (getf initargs (make-keyword (slot-definition-name slot-definition)))))
	 ;; The slot is required but the user didn't pass an initarg
	 ;; Trigger the error string
	 (error 'required-slot-error (error-msg slot-definition)))))

;; Some other designs:
;; 1) If the user doesn't provide an initform, then the slot is required. If he provides it, then it's not.
;; 2) Implement the error message as code in the :initform. We don't need a required-slots-object class neither a shared-initialize on it.
;; 3) Investigate more about metaclass composition and class redefinition and MOP


