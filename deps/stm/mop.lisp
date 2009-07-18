(in-package :stm)

;; MOP glue for STM

(defclass transactional-class (standard-class)
  ((transactional-slots :initform '()
			:accessor transactional-slots)))

(defclass transactional-object ()
  ((transactional-slots :initform (make-hash-table :test #'equal)
			:accessor transactional-slots)))


(defmethod validate-superclass ((class standard-class) (super transactional-class))
  t)

(defmethod validate-superclass ((class transactional-class) (super standard-class))
  t)

(defmethod initialize-instance :around ((class transactional-class) &rest initargs
					&key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'transactional-object)))
      ;; ',superclass is already one of the (indirect) superclasses
      (call-next-method)

      ;; ',superclass is not one of the superclasses, so we have to add it
      (apply #'call-next-method class
	     :direct-superclasses
	     (append direct-superclasses (list (find-class 'transactional-object))) initargs)))

(defmethod reinitialize-instance :around ((class transactional-class)
					  &rest initargs
					  &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p

      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses
	     thereis (subtypep class (find-class 'transactional-object)))
	  (call-next-method)
	  (apply #'call-next-method class
		 :direct-superclasses
		 (append direct-superclasses (list (find-class 'transactional-object))) initargs))
      
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method)))

(defclass transactional-slot-mixin ()
  ((transactional :initarg :transactional :initform t :accessor transactional-slot-p)))

(defmethod print-object ((slot-definition transactional-slot-mixin) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream " name: ~A transactional: ~A"
	    (slot-definition-name slot-definition)
	    (transactional-slot-p slot-definition))))

(defclass transactional-slot ()
  ((name :initarg :name :accessor transactional-slot-name)
   (owner :initarg :owner :accessor transactional-slot-owner)))

(defmethod value ((slot dataflow-slot))
  (slot-value (transactional-slot-owner slot) (intern (transaction-slot-name slot))))

(defmethod (setf value) (new-value (slot dataflow-slot))
  (setf (slot-value (dataflow-slot-owner slot) (intern (dataflow-slot-name slot))) new-value))

(defmethod print-object ((object transactional-slot) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream " slot-name: ~A owner: ~A"
	    (transactional-slot-name object)
	    (transactional-slot-owner object))))

(defclass transactional-direct-slot-definition
    (dataflow-slot-mixin standard-direct-slot-definition)
  ())

(defclass transactional-effective-slot-definition
    (dataflow-slot-mixin standard-effective-slot-definition)
  ())

(defun get-dataflow-slot (slot-name object)
  (assert (stringp slot-name))
  (gethash slot-name (dataflow-slots object)))

(defmethod shared-initialize :after ((obj transactional-object) slot-names &rest args)
  "Initialize the transactional slots of the object"
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
	    )))
  ; We can start to track events now
  (defmethod (setf slot-value-using-class) (new-value
					    (class dataflow-class)
					    object
					    slot-definition)
    (log-for mop "dataflow-class (setf slot-value-using-class) object: ~A :slotname ~A~%" object (slot-definition-name slot-definition))
    ;; Now notify the listeners of the change
    (let ((dataflow-slot (get-dataflow-slot (string (slot-definition-name slot-definition)) object))
	  (old-value (slot-value object (slot-definition-name slot-definition))))
      (log-for mop "Dataflow slot: ~A~%" dataflow-slot)
      (call-next-method)
      (when (not (funcall (dataflow-slot-test dataflow-slot) old-value new-value))
	(trigger-event :changed dataflow-slot new-value)
	;; Notify the object changed
	(trigger-event :changed object))))
)

(defmethod direct-slot-definition-class ((class dataflow-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'dataflow-direct-slot-definition))


(defmethod effective-slot-definition-class ((class dataflow-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'dataflow-effective-slot-definition))

(defmethod compute-effective-slot-definition :before ((class dataflow-class) slot-name direct-slots)
  (log-for mop "compute-effective-slot-definition slot: ~A slots: ~A~%" slot-name direct-slots)
  )

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

(defmacro with-df-slots (slots object &rest body)
  (let
      ((slots-gensyms (make-hash-table :test #'equal)))
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
       ,@body))))
