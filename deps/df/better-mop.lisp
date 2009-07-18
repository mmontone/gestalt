(in-package :df)

;; MOP glue for dataflow


(defclass dataflow-class (standard-class)
  ((dataflow-slots :initform '()
		   :accessor dataflow-slots)))

(defclass dataflow-object ()
  ((dataflow-slots :initform (make-hash-table :test #'equal)
		   :accessor dataflow-slots)
   (listeners :initform (make-hash-table :test #'equal) :accessor listeners)))


(defmethod validate-superclass ((class standard-class) (super dataflow-class))
  t)

(defmethod validate-superclass ((class dataflow-class) (super standard-class))
  t)

#|
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
  (log-for mop "Leaving: dataflow-class shared-initialize :around~%")
  )

|#


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

(defclass dataflow-slot (dfcell)
  ((name :initarg :name :accessor dataflow-slot-name)
   (owner :initarg :owner :accessor dataflow-slot-owner)
   (test :initarg :test :initform #'eq :reader dataflow-slot-test)
   ))

(defmethod value ((slot dataflow-slot))
  (slot-value (dataflow-slot-owner slot) (intern (dataflow-slot-name slot))))

(defmethod (setf value) (new-value (slot dataflow-slot))
  (setf (slot-value (dataflow-slot-owner slot) (intern (dataflow-slot-name slot))) new-value))

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

;; Redefining slot-value-using-class and keeping the old definition?

;; The more "reflective" option:
(defvar *old-method* nil)
(defvar *new-method* nil)

(let ((gf (symbol-function sb-mop:slot-value-using-class)))
  (setf *old-method* (sb-mop:compute-effective-method gf t
						      (sb-mop:compute-applicable-methods-using-classes (list (find-class 'dataflow-class) t t)))))
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

(setf *new-method* (sb-mop:compute-effective-method gf t
						      (sb-mop:compute-applicable-methods-using-classes (list (find-class 'dataflow-class) t t))))

;; When we want to change:
(add-method gf *new-method*)
(add-method gf *old-method*)

;; Note: this code is not tested at all. It's only a sketch of the idea.

;; With dataflow, we don't need full method redefinition. We can decide whether to do
;; track changes or not based on a dynamically bound variable:

(defvar *df-on* t)

(defmethod (setf slot-value-using-class) (new-value
					  (class dataflow-class)
					  object
					  slot-definition)
  (when (not *df-on*)
    (return (call-next-method)))
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

;; And we can provide a convenient macro:
(defmacro with-df-off (&rest body)
  (let ((*df-off* t))
    ,@body))
