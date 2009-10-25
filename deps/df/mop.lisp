(in-package :df)

;; MOP glue for dataflow

(defclass dataflow-class (standard-class)
  ()
  (:documentation "The dataflow objects metaclass"))

(defclass dataflow-object (cell)
  ((dataflow-slots :initform (make-hash-table :test #'equal)
		   :accessor dataflow-slots))
  (:documentation "Base class of every dataflow enhanced object. This class is automatically to the list of the object's superclasses by the dataflow-class metaclass"))

(defmethod validate-superclass ((class dataflow-class) (super standard-class))
  t)

(defmethod shared-initialize :around ((class dataflow-class) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from dataflow-object."
  (log-for mop "Entering: dataflow-class shared-initialize :around~%")
  (let* ((dataflow-metaclass (find-class 'dataflow-class))
	 (dataflow-object (find-class 'dataflow-object))
	 (not-already-dataflow (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) dataflow-metaclass))))
    (if (and (not (eq class dataflow-object)) not-already-dataflow)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list dataflow-object)) args)
	(call-next-method)))
  (log-for mop "Leaving: dataflow-class shared-initialize :around~%"))

(defclass dataflow-slot-mixin ()
  ((dataflow :initarg :dataflow :initform t :accessor dataflow-slot-p)
   (test :initarg :test :initform #'eq :accessor dataflow-slot-test)
   ))

(defmethod print-object ((slot-definition dataflow-slot-mixin) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream " name: ~A dataflow: ~A test: ~A"
	    (slot-definition-name slot-definition)
	    (dataflow-slot-p slot-definition)
	    (dataflow-slot-test slot-definition))))

(defclass dataflow-slot (cell)
  ((name :initarg :name :accessor dataflow-slot-name)
   (owner :initarg :owner :accessor dataflow-slot-owner)
   (test :initarg :test :initform #'eq :reader dataflow-slot-test)
   ))

(defmethod value ((slot dataflow-slot))
  (slot-value (dataflow-slot-owner slot) (intern (dataflow-slot-name slot))))

(defmethod (setf value) (new-value (slot dataflow-slot))
  (setf (slot-value (dataflow-slot-owner slot) (intern (dataflow-slot-name slot))) new-value)
  ;; Notify the slot changed
  (trigger-event 'changed
		 :triggerer slot
		 :value new-value))

(defmethod print-object ((object dataflow-slot) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream " slot-name: ~A owner: ~A test: ~A"
	    (dataflow-slot-name object)
	    (dataflow-slot-owner object)
	    (dataflow-slot-test object))))

(defclass dataflow-direct-slot-definition
    (dataflow-slot-mixin standard-direct-slot-definition)
  ())

(defclass dataflow-effective-slot-definition
    (dataflow-slot-mixin standard-effective-slot-definition)
  ())

(defun get-dataflow-slot (slot-name object)
  (assert (stringp slot-name))
  (gethash slot-name (dataflow-slots object)))

(defvar *df-mop* nil "Dataflow MOP active or not")

;; And we can provide a convenient macro:
(defmacro with-df-mop-off (&rest body)
  `(let ((*df-off* t))
    ,@body))

(defmethod shared-initialize :after ((obj dataflow-object) slot-names &rest args)
  "Initialize the dataflow slots of the object"
  (declare (ignore args))
  (log-for mop "shared-initialize :after dataflow-object slot-names: ~A~%" slot-names)
  (loop for slot in (class-slots (class-of obj))
     do
       (let ((slot-name (string (slot-definition-name slot))))
	 (assert (stringp slot-name))
	 (log-for mop "Initializing: ~A dataflow: ~A~%" slot (dataflow-slot-p slot) )
	 (when (and
		(dataflow-slot-p slot)
		(not (gethash slot-name (dataflow-slots obj))))
	   (setf (gethash slot-name (dataflow-slots obj))
		 (make-instance 'dataflow-slot
				:name slot-name
				:owner obj
				:test (dataflow-slot-test slot)))
	   ))))

(defmethod shared-initialize :around ((obj dataflow-object) slot-names &rest args)
  (declare (ignore obj slot-names args))
  (let ((*df-mop* nil))
    (call-next-method))
  (setf *df-mop* t))


(defmethod (setf slot-value-using-class) (new-value
					  (class dataflow-class)
					  object
					  slot-definition)
  (if *df-mop*
      (progn
	(log-for mop "dataflow-class (setf slot-value-using-class) object: ~A :slotname ~A~%" object (slot-definition-name slot-definition))
	;; Now notify the dependents of the change
	(let ((dataflow-slot (get-dataflow-slot (string (slot-definition-name slot-definition)) object)))
	  (log-for mop "Dataflow slot: ~A~%" dataflow-slot)
	  (call-next-method)
	  
	  (when (or (not (slot-boundp object (slot-definition-name slot-definition)))
		    (let ((old-value (slot-value object (slot-definition-name slot-definition))))
		      (not (funcall (dataflow-slot-test dataflow-slot) old-value new-value))))
	    
	    ;; Notify the object changed (no need for the object
	    ;; to register as a slot dependent)
	    (trigger-event 'changed
			   :triggerer object
			   :value object
			   ))))
      ;; else
      (call-next-method)))

(defmethod direct-slot-definition-class ((class dataflow-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'dataflow-direct-slot-definition))

(defmethod effective-slot-definition-class ((class dataflow-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'dataflow-effective-slot-definition))

(defmethod compute-effective-slot-definition :before ((class dataflow-class) slot-name direct-slots)
  (log-for mop "compute-effective-slot-definition slot: ~A slots: ~A~%" slot-name direct-slots))

(defmethod compute-effective-slot-definition ((class dataflow-class) name direct-slots)
   (let ((effective-slot (call-next-method)))
     (setf (dataflow-slot-p effective-slot)
           (some #'dataflow-slot-p direct-slots))
     (setf (dataflow-slot-test effective-slot)
	   (loop for direct-slot in direct-slots
	      when (dataflow-slot-test direct-slot)
		return (dataflow-slot-test direct-slot)))
     effective-slot))

(defmethod dataflow-slot-p ((object slot-definition))
  nil)

(defmethod dataflow-slot-test ((object slot-definition))
  nil)

(defmacro with-df-slots (slots object &body body)
  (let
      ((slots-gensyms (make-hash-table :test #'equal)))
    (once-only (object)
      `(let
       ,(loop for slot in slots
	   collect
	     (let
		 ((slot-gensym (gensym (string-upcase (string slot)))))
	       (setf (gethash slot slots-gensyms) slot-gensym)
	       `(,slot-gensym (get-dataflow-slot ,(string slot) ,object))))
     (symbol-macrolet
	 ,(loop for slot in slots
	       collect `(,slot ,(gethash slot slots-gensyms)))
       ,@body)))))