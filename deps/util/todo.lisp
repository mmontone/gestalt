;; Consider defining a code walker protocol using the MOP machinery

;; Here is an sketch:

;; Consider make-method-lambda for declaring a walker's special functions.

(in-package :gst.util)

(defclass walker-generic-function-class (funcallable-standard-class)
  ())

(define-condition walker-function-definition-error ()
  ())

(defmacro def-walker-case (case args &body body)
  "We want walker functions to be defined as instances of walker-generic-function-class because we want to manipulate its methods body using the make-method-lambda machinery"
  (let ((method-name (symbol-name case)))
    (with-unique-names (generic-function-class)
      `(let ((,generic-function-class (find-class ',case nil)))
	 (when (not ,generic-function-class)
	   (defgeneric ,case ,args
	     (:metaclass walker-generic-function-class))
	   (setf ,generic-function-class (find-class 'name)))
	 (if (typep ,generic-function-class 'walker-generic-function-class)
	     (defmethod ,name ,args
	       ,@body)
	     (error 'walker-function-definition-error))))))

(defmethod make-method-lambda ((gf walker-generic-function-class) method lambda-expression environment)
  "Apply transformation to lambda-expression")

(defmethod initialize-instance :after ((class walker-generic-function-class) &rest initargs)
  "Ensure walker-generic-function is a super class of class"
  
  )

(defclass standard-code-walker (code-walker)
  ())

;; The cases should be evaluated in order

;; A constant or keyword
(def-code-walker-case standard-code-walker
    (or :constantp :keywordp)
  (form)
  form)

;; A lexical variable
(def-code-walker-case :lexical-var ((walker standard-code-walker) form)
  form)

;; A dynamic variable
(def-code-walker-case :special-var ((walker standard-code-walker) form)
  form)


;; A free variable
(def-code-walker-case :free-var ((walker standard-code-walker) form)
  form)


;; A lambda expression
(def-code-walker-case (lambda args &body body) ((walker standard-code-walker) form lexenv)
  (let ((new-lexenv (append (list-lambda-list-vars args)
			    lexenv)))
      `(lambda ,args
	 ,@(mapcar (lambda (body-form)
		     (funcall walker body-form
			      new-lexenv))
		   body))))

;; A let expression
(def-code-walker-case (let bindings &rest body) ((walker standard-code-walker) form lexenv)
    (let* ((bindings (loop for binding in bindings
		      collect (destructuring-bind (binding-name binding-expr) binding
				(list binding-name
				      (funcall walker binding-expr
					       lexenv)))))
	   (new-lexenv (append (mapcar #'car bindings) lexenv)))
    `(let ,bindings
       ,@(mapcar (lambda (body-form)
		  (funcall walker body-form
			   new-lexenv))
		     body))))

;; A let* expression
(def-code-walker-case (let* bindings &rest body) ((walker standard-code-walker) form lexenv)
  (let* ((new-lexenv lexenv)
	 (bindings (loop for binding in bindings
		      collect (destructuring-bind (binding-name binding-expr) binding
				(aprog1
				    (list binding-name
					  (funcall walker binding-expr
						   new-lexenv))
				  (push (car it) new-lexenv)
				  it)))))
    `(let* ,bindings
       ,@(mapcar (lambda (body-form)
		  (funcall walker body-form
				  new-lexenv))
		body))))

;; An flet expression
(def-code-walker-case (flet bindings &rest body) ((walker standard-code-walker) form lexenv)
    (let* ((new-lexenv '())
	   (bindings (loop for binding in bindings
			collect
			  (destructuring-bind
				(f-name f-args &rest f-body) binding
			    (let* ((local-lexenv (append lexenv
							 (list-lambda-list-vars f-args)))
				   (new-binding `(,f-name ,f-args
							  ,@(mapcar (lambda (body-form)
								    (funcall walker body-form
										   local-lexenv))
								  f-body))))
			      (push f-name new-lexenv)
			      new-binding)))))
      (let ((lexenv (append new-lexenv lexenv)))
	`(flet ,bindings
	   ,@(mapcar (lambda (body-form)
		       (funcall walker body-form
				new-lexenv))
		     body)))))

;; A labels expression
(def-code-walker-case (labels bindings &rest body) ((walker standard-code-walker) form lexenv)
    (let* ((new-lexenv '())
	   (bindings (loop for binding in bindings
			collect
			  (destructuring-bind
				(f-name f-args &rest f-body) binding
			    (let* ((local-lexenv (append new-lexenv
							 (list-lambda-list-vars f-args)
							 (list f-name)))
				   (new-binding `(,f-name ,f-args
							  ,@(mapcar (lambda (body-form)
								    (funcall walker body-form
										   local-lexenv))
								f-body))))
			      (push f-name new-lexenv)
			      new-binding)))))
      (let ((new-lexenv (append new-lexenv lexenv)))
	`(labels ,bindings
	   ,@(mapcar (lambda (body-form)
		       (funcall walker body-form
				      new-lexenv))
		  body)))))

(def-code-walker-case (quote form) ((walker standard-code-walker) whole-form lexenv)
  (declare (ignore form lexenv))
  whole-form)

(def-code-walker-case (return-from name &optional expr) ((walker standard-code-walker) form lexenv)
  `(return-from ,name ,(funcall walker expr lexenv)))

(def-code-walker-case (declare form) ((walker standard-code-walker) whole-form lexenv)
  (declare (ignore form lexenv))
  whole-form)

;; If we could use MOP machinery, then we could use defmethod for
;; walker cases, and could inherit from standard-walker (just walks recursively). In that case, we could define freevars-replacer-walker as a subclass of standard-code-walker and define the case for free variables only
(defclass freevars-replacer-walker (standard-code-walker)
  ((replace-function
    :initarg :replace-function
    :required t
    :documentation "The function used to replace freevariables"))
  (:metaclass required-slots-class))

(def-code-walker-case :free-var ((walker freevars-replacer-walker) form &optional lexenv)
  (funcall (replace-function walker) form))

(defmacro def-code-walker-case (pattern-spec args &body body)
  (destructuring-bind (walker-spec form lexenv) args
    (destructuring-bind (walker-var walker-class) walker-spec
      (let ((pattern (get-pattern pattern-spec)))
      (with-unique-names (walker)
	`(let ((,walker (get-singleton ',walker-class)))
	   (add-case ,walker
		     (make-instance 'code-walker-case
				    :pattern ,pattern
				    :function (lambda (,form ,lexenv)
						(funcall ,(inject-function pattern)
							 (lambda ,(funcall (extract-function pattern) ',pattern-spec)
							   ,@body)
							 ,form))))))))))

(define-condition no-applicable-code-walker-case-error (serious-condition)
  ())

(defmethod get-applicable-code-walker-case ((code-walker code-walker) form)
  (loop for walker in (sb-mop:class-precedence-list code-walker)
       while (not (eql walker (find-class 'code-walker)))
       do (loop for walker-case in (walker-cases walker)
	       when (matches walker-case form)
	       do (return-from get-code-walker-case walker-case)))
  (error 'non-applicable-code-walker-case-error))
 							 
(defgeneric walk (walker form)
  (:metaclass :walker-generic-function-class))

;; walker-generic-function-class should influentiate both in how a walk method body is compiled and how walk methods are defined (to include the pattern, for example). The method combination should be append or progn or something like that (execute the cases in order of declaration).

;; make-method-lambda example: http://www.lisp.org/mop/dictionary.html#make-method-lambda

;; We can achieve something similar wrapping defmethod calls with a macro and modifying the body with a macroexpansion

(defun replace-freevars (form &optional (lexenv nil))
  (funcall (get-singleton 'freevars-replacer) form lexenv))

;; IMPORTANT: we do the pattern mathing explicitily and extensibly defining walker-patterns. We do not do pattern matching!! :) We can optionally use pattern matching in the match-function

(defvar *code-walker-patterns* '() :documentation "List of code walker defined patterns")

(defclass code-walker-pattern ()
  ((name :initarg :name
	 :accessor pattern-name
	 :required t
	 :documentation "The name of the pattern")
   (hash-function :initarg :hash-function
		  :accessor hash-function
		  :required t
		  :documentation "The function for distinguishing this pattern from another")
   (match-function :initarg :match-function
		   :accessor match-function
		   :required t
		   :documentation "The function that takes a form and tells if matches this pattern")
   (inject-function :initarg :inject-function
		    :accessor inject-function
		    :required t
		    :documentation "The function that takes a functions and a matched form and injects is parts in the former")
   (extract-function :initarg :extract-function
		     :accessor extract-function
		     :required t
		     :documentation "The function that takes a pattern and extracts the list of formal arguments")
   (documentation :initarg :documentation
		  :accessor documentation
		  :documentation "The documentation of the pattern"))
  (:metaclass required-slots-class))


(defmethod initialize-instance :after ((code-walker-pattern code-walker-pattern &rest initargs))
  (push code-walker-pattern *code-walker-patterns*))

(defmacro define-code-walker-pattern (&key pattern-name
				           hash-function
				           match-function
				           inject-function
				           extract-function
				      (documentation "pattern not documented"))
  `(make-instance 'code-walker-pattern
		  :name ,pattern-name
		  :hash-function ,hash-function
		  :match-function ,match-function
		  :inject-function ,inject-function
		  :extract-function ,extract-function
		  :documentation ,documentation))
    
(define-code-walker-pattern
	       :name "(let bindings &rest body)"
               :hash-function (lambda (spec)
				(equalp (car spec) 'let))
	       :match-function (lambda (form)
				 (equalp (car form) 'let))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (let bindings &rest body) form
	       			    (declare (ignore let))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (spec)
				   (destructuring-bind (let bindings rest-keyword body) spec
				     (declare (ignore let rest-keyword))
				     (list bindings body)))
	       :documentation "Matches a let form")

(define-code-walker-pattern
	       :name "(let* bindings &rest body)"
               :hash-function (lambda (spec)
				(equalp (car spec 'let*)))
	       :match-function (lambda (form)
				 (equalp (car form) 'let*))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (let* bindings &rest body) form
	       			    (declare (ignore let*))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (pattern)
				   (destructuring-bind (let* bindings rest-keyword body) pattern
				     (declare (ignore let* rest-keyword))
				     (list bindings body)))
	       :documentation "Matches a let* form")

(define-code-walker-pattern
	       :name "(flet bindings &rest body)"
               :hash-function (lambda (spec)
				(equalp (car spec) 'flet))
	       :match-function (lambda (form)
				 (equalp (car form) 'flet))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (flet bindings &rest body) form
	       			    (declare (ignore flet))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (pattern)
				   (destructuring-bind (flet bindings rest-keyword body) pattern
				     (declare (ignore flet rest-keyword))
				     (list bindings body)))
	       :documentation "Matches an flet form")

(define-code-walker-pattern
	       :name "(labels bindings &rest body)"
               :hash-function (lambda (spec)
				(equalp (car spec) 'labels))
	       :match-function (lambda (form)
				 (equalp (car form) 'labels))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (labels bindings &rest body) form
	       			    (declare (ignore labels))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (pattern)
				   (destructuring-bind (labels bindings rest-keyword body) pattern
				     (declare (ignore labels rest-keyword))
				     (list bindings body)))
	       :documentation "Matches an labels form")


(define-code-walker-pattern
	       :name "*special-variable*"
               :hash-function (lambda (spec) (equalp spec :special-variable))
	       :match-function #'dynamic-variable-symbol-p
	       :inject-function #'funcall
	       :extract-function (lambda (pattern) nil)
	       :documentation "Matches a special variable")

(define-code-walker-pattern
    :name "(quote expression)"
  :hash-function (lambda (spec) (equalp (car spec) 'quote))
  :match-function (lambda (form) (equalp (car form) 'quote))
  :inject-function (lambda (where form) (funcall where (cadr form)))
  :extract-function #'cadr
  :documentation "Matches a quoting form")

(defmethod print-object ((code-walker-pattern code-walker-pattern) stream)
  (print-unreadable-object (code-walker-pattern stream :type t :identity t)
    (format stream "~A" (pattern-name code-walker-pattern))))

(defclass code-walker-class (required-slots-class singleton-class funcallable-standard-class)
  ())
  
(defclass code-walker ()
  ((name :initarg :name
	 :accessor name
	 :required t
	 :documentation "The walker's name")
   (cases :initform '()
	  :accessor :cases
	  :documentation "The cases the walker handles"))
  (:metaclass code-walker-class))

(defclass code-walker-case ()
  ((pattern :initarg :pattern
	    :accessor pattern
	    :required t
	    :documentation "The pattern to match")
   (body
    :initarg :body
    :accessor body
    :required t
    :documentation "The function to call on the matching form"))
  (:metaclass required-slots-class))

(defmethod matches-p ((case code-walker-case) form)
  (funcall (pattern case) form))

(defmethod apply-case ((case code-walker-case) form)
  (funcall (body case) form))

(defmethod run-case ((case code-walker-case) form)
  (aprog1
      (matches-p case form)
    (when it
      (apply-case case form))))

(defmethod initialize-instance :after ((code-walker code-walker) &key)
  "Walker as funcallable instances"
  (set-funcallable-instance-function c
				     #'(lambda (form)
					 (apply-code-walker code-walker form))))

(defmethod apply-code-walker ((code-walker code-walker) form)
  (loop for case in (cases code-walker)
     when (matches-p case form)
     do (return-from apply-code-walker
	  (apply-case case form)))
  ;; There was no matching. Apply the code walker to subexpressions
  (cons (car form)
	(mapcar (lambda (form)
		  (apply-code-walker code-walker form))
		(cdr form))))

(defmethod print-object ((code-walker code-walker) stream)
  (print-unreadable-object (code-walker stream :type t :identity t)
    (format stream "~A" (name code-walker))))

(defmacro defwalker (name)
  (let* ((name-string (symbol-name name))
	 (walker-class-name (concatenate 'string name-string "-walker"))
	 (with-macro-name (intern (concatenate 'string "with-" name-string))))
    `(defmacro ,with-macro-name (&body body)
       (run-walker (find-singleton ,walker-class-name)
		   `(progn ,@body)))))
	   

(defmacro def-walkercase (walker-name pattern args &rest body))

(def-walkercase (let bindings &rest body) ()

  )

(defun run-walker (walker form)
  (funcall walker form))