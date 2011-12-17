(in-package :code-walker)

(defcategory cw
    ()
  "Category for logging code-walker actions")

(start-sender 'cw
  (stream-sender :location *error-output*)
  :category-spec '(cw)
  :output-spec '(message))


(defvar *code-walker-patterns* '() "Defined code walker patterns")

(defclass code-walker ()
  ((name :initarg :name
	 :accessor name
	 :required t
	 :documentation "The walker's name")
   (cases :initform '()  ;; TODO:  Use a generic-hash-table instead, with :test #'typep on the cases patterns??
	  :accessor code-walker-cases
	  :documentation "The cases the walker handles")
   (parent :initform nil
	   :accessor parent
	   :initarg :parent
	   :documentation "The parent walker. As long as we don't work at the meta level, we need this variable"))
  (:metaclass required-slots-class))

(defclass code-walker-pattern ()
  ((name :initarg :name
	 :accessor pattern-name
	 :required t
	 :documentation "The name of the pattern")
   (spec-matching-function :initarg :spec-matching-function
			   :accessor spec-matching-function
			   :required t
			   :documentation "The function that tells a pattern from a spec")
   (form-matching-function :initarg :form-matching-function
			   :accessor form-matching-function
			   :required t
			   :documentation "The function that takes a form and tells if matches this pattern")
   (inject-function :initarg :inject-function
		    :accessor inject-function
		    :required t
		    :documentation "The function that takes a function and a matched form and injects is parts in the former")
   (extract-function :initarg :extract-function
		     :accessor extract-function
		     :required t
		     :documentation "The function that takes a pattern and extracts the list of formal arguments")
   (doc :initarg :doc
	:accessor code-walker-pattern-doc
	:documentation "The documentation of the pattern"))
  (:metaclass required-slots-class))

(defclass code-walker-case ()
  ((pattern :initarg :pattern
	    :accessor code-walker-case-pattern
	    :required t
	    :documentation "The pattern to match")
   (code-walker :initarg :code-walker
		:accessor code-walker
		:documentation "The code-walker this object is attached to")
   (function :initarg :function
	     :accessor code-walker-case-function
	     :required t
	     :documentation "The function to call on the matching form"))
  (:metaclass required-slots-class))


(define-condition no-applicable-code-walker-case-error (serious-condition)
  ((spec :initarg :spec :reader spec)))

(define-condition pattern-not-found-error (serious-condition)
  ((spec :initarg :spec :reader spec)))

(defmacro define-code-walker-case (pattern-spec (code-walker form lexenv) &body body)
  (let ((extracted-args (extract-arguments (get-code-walker-pattern pattern-spec) pattern-spec)))
    (with-unique-names (pattern walker)
      `(let ((,pattern (get-code-walker-pattern ',pattern-spec)))
	 (add-code-walker-case ,code-walker
			       (make-instance 'code-walker-case
					      :pattern ,pattern
					      :function (lambda (,walker ,form ,lexenv)
							  (flet ((walk (form lexenv)
								       (apply-code-walker ,walker form lexenv)))
							    (inject-arguments ,pattern
									      ,form
									      (lambda ,extracted-args
										,@body))))))))))
(defmacro define-code-walker-pattern (name &key
				      spec-matching-function
				      form-matching-function
				      inject-function
				      extract-function
				      (doc "pattern not documented"))
  `(add-code-walker-pattern
    (make-instance 'code-walker-pattern
		   :name ',name
		   :spec-matching-function ,spec-matching-function
		   :form-matching-function ,form-matching-function
		   :inject-function ,inject-function
		   :extract-function ,extract-function
		   :doc ,doc)))

(defmethod print-object ((code-walker code-walker) stream)
  (print-unreadable-object (code-walker stream :type t :identity t)
    (format stream "~A" (name code-walker))))

(defun extract-arguments (code-walker-pattern pattern-spec)
  (funcall (extract-function code-walker-pattern) pattern-spec))

(defun inject-arguments (code-walker-pattern form function)
  (funcall (inject-function code-walker-pattern) function form))

(defun get-code-walker-pattern (spec &optional (errorp t))
  (loop for code-walker-pattern in *code-walker-patterns*
     when (matches-spec code-walker-pattern spec)
     do (return-from get-code-walker-pattern code-walker-pattern))
  (when errorp
    (error 'pattern-not-found-error :spec spec)))

(defmethod print-object ((err no-applicable-code-walker-case-error) stream)
  (print-unreadable-object (err stream :type t :identity t)
    (format stream "~A" (spec err))))

(defun code-walker-parent-list (code-walker)
  (if (null code-walker)
      nil
      (cons code-walker
	    (code-walker-parent-list (parent code-walker)))))

(defmethod get-applicable-code-walker-case ((code-walker code-walker) form lexenv)
  (log-for cw "~A : Looking for applicable case for ~A under ~A" code-walker form lexenv)
  (loop for cw in (code-walker-parent-list code-walker)
       do (loop
	     for walker-case in (code-walker-cases cw)
	     when (code-walker-case-matches walker-case form lexenv)
	     do (progn
		  (log-for cw "~A : Case found: ~A" code-walker walker-case)
		  (return-from get-applicable-code-walker-case walker-case))))
  (error 'no-applicable-code-walker-case-error :spec form))


(defmethod code-walker-pattern-hash ((pattern code-walker-pattern))
  "Overwrite over an eql specializer"
  (pattern-name pattern))

(defun add-code-walker-pattern (code-walker-pattern)
  (push code-walker-pattern *code-walker-patterns*))

(defun remove-code-walker-pattern (code-walker-pattern)
  (setf *code-walker-patterns* (remove code-walker-pattern *code-walker-patterns*)))

(defmethod print-object ((err pattern-not-found-error) stream)
  (print-unreadable-object (err stream :type t :identity t)
    (format stream "~A" (spec err))))

(defmethod print-object ((code-walker-pattern code-walker-pattern) stream)
  (print-unreadable-object (code-walker-pattern stream :type t :identity t)
    (format stream "~A" (pattern-name code-walker-pattern))))

(defmethod matches-spec ((pattern code-walker-pattern) spec)
  (funcall (spec-matching-function pattern) spec))

(defmethod matches-form ((pattern code-walker-pattern) form lexenv)
  (funcall (form-matching-function pattern) form lexenv))

(defmethod matches-form ((pattern symbol) form lexenv)
   (matches-form (get-code-walker-pattern pattern) form lexenv))

(defmethod matches-form ((pattern cons) form lexenv)
  (matches-form (get-code-walker-pattern pattern) form lexenv))

(defmethod apply-code-walker ((code-walker code-walker) form &optional lexenv)
  (let
      ((code-walker-case (get-applicable-code-walker-case code-walker form lexenv)))
    (apply-code-walker-case code-walker-case code-walker form lexenv)))

(defmethod print-object ((code-walker-case code-walker-case) stream)
  (print-unreadable-object (code-walker-case stream :type t :identity t)
    (format stream "~A" (pattern-name (code-walker-case-pattern code-walker-case)))))

(defmethod code-walker-case-matches ((case code-walker-case) form lexenv)
  (matches-form (code-walker-case-pattern case) form lexenv))

(defmethod apply-code-walker-case ((case code-walker-case) (code-walker code-walker) form lexenv)
  (funcall (code-walker-case-function case) code-walker form lexenv))

(defmethod add-code-walker-case ((code-walker code-walker) (code-walker-case code-walker-case))
  (setf (code-walker code-walker-case) code-walker)
  (push code-walker-case (code-walker-cases code-walker)))

(defmethod run-code-walker-case ((case code-walker-case) (code-walker code-walker) form lexenv)
  (aprog1
      (code-walker-case-matches case form lexenv)
    (when it
      (values (apply-code-walker-case case code-walker form lexenv) t)
      (values nil nil))))