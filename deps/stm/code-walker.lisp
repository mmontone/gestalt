(defvar *access-stm-var* t "Tells if we should access stm-vars through the value method, or not")

(defclass stm-code-walker (code-walker)
  ((stm-functions :initform '(make-hash-table :test #'equalp)
		  :accessor stm-functions)))

(defmethod get-stm-function (name (code-walker stm-code-walker))
  (gethash name (stm-functions code-walker)))

(defmethod add-stm-function (name (code-walker stm-code-walker) function)
  (setf (gethash name (stm-function code-walker))
	(make-instance 'stm-function
		       :name name
		       :function function)))

(defmethod initialize-instance :after ((code-walker stm-code-walker) &rest initargs)
  (setf (parent code-walker) *standard-codewalker*)

  (define-code-walker-pattern (declare (external vars))
      :spec-matching-function (lambda (spec)
				(and (listp spec)
				     (equalp (car spec) 'declare)
				     (and (listp (cdr spec))
					  (equalp (cadr spec) 'external))))
      :form-matching-function (lambda (form)
				(and (listp form)
				     (equalp (car form) 'declare)
				     (and (listp (cdr form))
					  (equalp (cadr form) 'external))))
      :inject-function #'funcall
      :extract-function (compose #'list #'cddr))

  (define-code-walker-pattern (stm-f args)
      :spec-matching-function (lambda (spec)
				(and (listp spec)
				     (equalp (car spec) 'stm-f)))
      :form-matching-function (lambda (form)
				(and (listp form)
				     (multiple-value-bind (fun foundp)
					 (get-stm-fun code-walker (car form))
				       (declare (ignore fun))
				       foundp)))
      :inject-function (lambda (where form)
			 (funcall where (car form) (cdr form)))
      :extract-function #'identity)

  (define-code-walker-case (declare (external vars)) (code-walker form lexenv)
    (loop for var in vars
	 do (augment-lexenv lexenv var)))

  (define-code-walker-case (stm-f args) (code-walker form lexenv)

    (walk
     `(funcall (get-stm-function ',stm-f ,code-walker)

     lexenv))

  (define-code-walker-case ())
  )

(defclass stm-function ()
  ((name :initarg :name
	 :accessor stm-function-name
	 :initform (error "Provide the function name"))
   (args :initarg :args
	 :accessor stm-function-args
	 :initform '())
   (function :initarg :function
	     :accessor stm-function-function
	     :initform (error "Provide the function")))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((stm-function stm-function) &rest initargs)
  (set-funcallable-instance-function stm-function
				     (lambda (&rest args)
				       (apply (stm-function-function stm-function) args))))

(defmethod print-object ((stm-function stm-function) stream)
  (print-unreadable-object (stm-function stream :type t :identity t)
    (format stream "~A" (stm-function-name stm-function))))
