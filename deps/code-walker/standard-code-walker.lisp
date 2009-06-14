(in-package :code-walker)

(defvar *standard-code-walker*
  (make-instance 'code-walker :name 'standard-code-walker))

;; The cases have to be added in order, from less specific to more specific

(define-code-walker-case (f args) (*standard-code-walker* form lexenv)
  (declare (ignore form lexenv))
  `(,f ,@(mapcar (lambda (arg) (walk arg lexenv))
		 args)))

;; A lambda expression
(define-code-walker-case (lambda args &body body) (*standard-code-walker* form lexenv)
  (let ((new-lexenv (append (list-lambda-list-vars args)
			    lexenv)))
      `(lambda ,args
	 ,@(mapcar (lambda (body-form)
		     (walk body-form
			   new-lexenv))
		   body))))

;; A let expression
(define-code-walker-case (let bindings &rest body) (*standard-code-walker* form lexenv)
    (let* ((bindings (loop for binding in bindings
		      collect (destructuring-bind (binding-name binding-expr) binding
				(list binding-name
				      (walk binding-expr
					    lexenv)))))
	   (new-lexenv (append (mapcar #'car bindings) lexenv)))
    `(let ,bindings
       ,@(mapcar (lambda (body-form)
		  (walk body-form
			new-lexenv))
		     body))))

;; A let* expression
(define-code-walker-case (let* bindings &rest body) (*standard-code-walker* form lexenv)
  (let* ((new-lexenv lexenv)
	 (bindings (loop for binding in bindings
		      collect (destructuring-bind (binding-name binding-expr) binding
				(aprog1
				    (list binding-name
					  (walk binding-expr
						new-lexenv))
				  (push (car it) new-lexenv)
				  it)))))
    `(let* ,bindings
       ,@(mapcar (lambda (body-form)
		  (walk body-form
			new-lexenv))
		 body))))

;; An flet expression
(define-code-walker-case (flet bindings &rest body) (*standard-code-walker* form lexenv)
    (let* ((new-lexenv '())
	   (bindings (loop for binding in bindings
			collect
			  (destructuring-bind
				(f-name f-args &rest f-body) binding
			    (let* ((local-lexenv (append lexenv
							 (list-lambda-list-vars f-args)))
				   (new-binding `(,f-name ,f-args
							  ,@(mapcar (lambda (body-form)
								    (walk body-form
									  local-lexenv))
								    f-body))))
			      (push f-name new-lexenv)
			      new-binding)))))
      (let ((lexenv (append new-lexenv lexenv)))
	`(flet ,bindings
	   ,@(mapcar (lambda (body-form)
		       (walk body-form
			     new-lexenv))
		     body)))))

;; A labels expression
(define-code-walker-case (labels bindings &rest body) (*standard-code-walker* form lexenv)
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
								    (walk body-form
									  local-lexenv))
								    f-body))))
			      (push f-name new-lexenv)
			      new-binding)))))
      (let ((new-lexenv (append new-lexenv lexenv)))
	`(labels ,bindings
	   ,@(mapcar (lambda (body-form)
		       (walk body-form
			     new-lexenv))
		     body)))))

(define-code-walker-case (quote form) (*standard-code-walker* whole-form lexenv)
  (declare (ignore form lexenv))
  whole-form)

(define-code-walker-case (return-from name &optional expr) (*standard-code-walker* form lexenv)
  `(return-from ,name ,(walk expr lexenv)))

(define-code-walker-case (declare form) (*standard-code-walker* whole-form lexenv)
  (declare (ignore form lexenv))
  whole-form)

;; A constant or keyword
(define-code-walker-case :keyword (*standard-code-walker* form lexenv)
  form)

;; A lexical variable
(define-code-walker-case :lexvar (*standard-code-walker* form lexenv)
  form)

;; A dynamic variable
(define-code-walker-case *special-variable* (*standard-code-walker* form lexenv)
  form)

;; A free variable
(define-code-walker-case :freevar (*standard-code-walker* form lexenv)
  form)

(define-code-walker-case :number (*standard-code-walker* form lexenv)
  form)