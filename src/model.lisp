(defpackage consistent-object
  (:use :cl :closer-mop)
  (:export
   #:consistent-object-class
   #:consistency-check
   #:consistency-error
   #:validate
   #:ensure
   #:suspending-consistency))

(defvar *modification-transaction* nil)

(defclass consistent-object-class ()
  ()
  (:documentation "Metaclass for consistent objects"))

(defclass consistent-object ()
  ()
  (:documentation "Superclass of every consistent object"))

(define-condition consistency-error (simple-error)
  ()
  (:documentation "Error that is raised when a model object consistency error occurs"))

#+consistency-check
(defmethod (setf name) :around (value (person person))
  (if *modification-transaction*
      (call-next-method)
      (progn
	(call-next-method)
	(validate person))))

;; For the following we need a new method combination that lets us attach
;; several :before and :after methods to a :primary method. That makes
;; it more compositional, although more difficult to redefine and remove
;; methods.

#+consistency-check
(defmethod initialize-instance :after ((person person) &rest initargs)
  (validate person))

(defun consistency-error (datum &rest args)
  (apply #'error 'consistency-error args))

(defmacro with-modification-transaction (objects &body body)
  `(let ((*modification-transaction* t))
     ,@body
     ,@(loop for object in objects
	  collect `(validate ,object))))

(defmethod validate ((person person))
  (check (and (stringp (name person))
	      (> (length (name person)) 0)))
  (check (and (stringp (lastname person))
	      (> (length (lastname person)) 0))))
  
;; Example:
;; The following fails:
(make-instance 'person)

;; The following fails:
(with-modification-transaction (person)
  (let ((person (make-instance 'person)))
    (setf (name person) "Mariano")))

;; The following fails:
(let ((person (make-instance 'person)))
    (setf (name person) "Mariano")
    (setf (lastname person) "Montone"))

;; The following works:  
(with-modification-transaction (person)
  (let ((person (make-instance 'person)))
    (setf (name person) "Mariano")
    (setf (lastname person) "Montone")))

;; Objects versioning

(defmethod new-object-p (object)
  (let ((id (id object)))
    (or (null id) (zerop id))))

(def-view-class versionable-object ()
  ((version :initarg :version
	    :accessor version
	    :initform 0
	    :type integer)))

(define-condition object-version-conflict ()
  ((object :initarg :object :accessor object))
  (:report (lambda (self stream)
	     (format stream "There's version conflict when trying to store ~A"(object self)))))

(defmethod store :around ((object versionable-object))
  (if (new-object-p object)
      (progn
	(setf (version object) 1)
	(call-next-method))
      (let ((stored-object (find-object (class-name (class-of object))
					(id object))))
	(if (equal (version stored-object) (version object))
	    (progn
	      (incf (version object))
	      (call-next-method))
	    (restart-case
		(error 'object-version-conflict :object object)
	      (continue ()
		:report (lambda (stream)
			  (format stream "Store the object anyway"))
		(call-next-method)))))))

;; Observation: we could implement objects versioning as yet another metaclass and combine it with persistent-class. The metaclass would simply install the version-object superclass.