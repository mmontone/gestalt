(in-package :consistent-object)

(defvar *suspend-consistency* nil)

(defclass consistent-object-class (standard-class)
  ()
  (:documentation "Metaclass for consistent objects"))

(defclass consistent-object ()
  ()
  (:documentation "Superclass of every consistent object"))

(define-condition consistency-error (serious-condition)
  ((target :initarg :target
	   :reader target)
   (messages :initarg :messages
	     :accessor messages
	     :initform '()))
  (:report (lambda (error stream)
	     (if (target error)
		 (format stream "~A is inconsistent. Reasons: ~A" (target error) (messages error))
		 (format stream "Inconsistency error. Reasons: ~A" (messages error)))))
  (:documentation "Error that is raised when a model object consistency error occurs"))
  
(defun consistency-error (target datum &rest args)
  (with-simple-restart (continue "Continue in spite of consistency error")
    (error 'consistency-error :target target
	   :messages (list (apply #'format nil datum args)))))

(defmethod message ((error consistency-error))
  (first (messages error)))

(defmethod (setf message) (message (error consistency-error))
  (push message (messages error)))

(defmethod validate-superclass ((class consistent-object-class) (super standard-class))
  t)

(defmethod shared-initialize :around ((class consistent-object-class) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from dataflow-object."
  (let* ((consistent-object-metaclass (find-class 'consistent-object-class))
	 (consistent-object-class (find-class 'consistent-object))
	 (not-already-consistent-object (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) consistent-object-metaclass))))
    (if (and (not (eq class consistent-object-class)) not-already-consistent-object)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list consistent-object-class)) args)
	(call-next-method))))

#-no-consistency-check
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defmacro suspending-consistency-for (objects &body body)
      (let ((result (gensym "RESULT-")))
	`(let (,result)
	   (let ((*suspend-consistency* t))
	     (setf ,result (progn ,@body)))
	   ,@(loop for object in objects
		collect `(validate ,object))
	   ,result))))
  (defmethod (setf slot-value-using-class) (new-value
					    (class consistent-object-class)
					    object
					    slot-definition)
    (call-next-method)
    (when (not *suspend-consistency*)
      (validate object)))
  ;; For the following we need a new method combination that lets us attach
  ;; several :before and :after methods to a :primary method. That makes
  ;; it more compositional, although more difficult to redefine and remove
  ;; methods.
  ;;(defmethod initialize-instance :after ((person person) &rest initargs)
  ;;  (validate person))
  ;; I'll overwrite with an :around method for the moment:
  (defmethod initialize-instance :around ((object consistent-object) &rest initargs)
    (declare (ignore initargs))
    (suspending-consistency-for (object)
      (call-next-method))))

#+no-consistency-check
(defmacro suspending-consistency-for (objects &body body)
  ,@body)

(defgeneric validate (object)
  (:method ((object consistent-object))
    object)
  (:method :around ((object consistent-object))
	   (when (not *suspend-consistency*)
	     (let ((messages '())
		   (occurred-error-p nil)
		   result)
	       (handler-bind ((consistency-error
			       (lambda (error)
				 (setf occurred-error-p t)
				 (push (first (messages error)) messages)
				 (continue))))
		 (setf result (call-next-method)))
	       (when occurred-error-p
		 (consistency-error object "~{~a~^ and ~}" messages))
	       result))))
  
(defun ensure (flag datum &rest initargs)
  (if (not flag)
      (apply #'consistency-error nil datum initargs)
      t))