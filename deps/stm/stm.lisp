(in-package :stm)

(defvar *debug* t "Turn off to disable debugging macros. Note you'll have to recompile your code in order for this to be effective")

(defmacro defdbgmacro (name args &rest body)
  `(defmacro ,name ,args
     (when *debug*
	 ,@body)))

(defdbgmacro trk (datum &rest args)
  `(when *track*
     (format t ,datum ,@args)))

(defdbgmacro dbg (&body body)
  `(progn
     ,@body))

(defcategory stm)
(defcategory none)

#|
(start-sender 'stm
  (stream-sender :location *error-output*)  
  :category-spec '(stm)
  :output-spec '(message))

(start-sender 'none
  (stream-sender :location *error-output*)  
  :category-spec '(none)
  :output-spec '(message))

|#

(defvar *stm-transaction* nil "The current stm transaction")


(defun stm-transaction ()
  "We wrap the current stm transaction in a function because we want more control on its scope than what is provided by a special variable. For example, we may want to dynamically bind this function throgh contextl dflet to modify the way we return the current transaction, always taking into account the dynamic environment and its posible restoring"
  *stm-transaction*)

;; We want optimistic semantics. We effectivly change the value of each of the stm variables references on each read or write. We "do nothing" on commit, we rollback variable values back on rollback.

(defclass stm-transaction ()
  ((changes :initform (make-hash-table)
	    :accessor changes
	    :documentation "A hash table with the changes of each variable")
   (parent :initarg :parent
	   :reader parent
	   :initform nil
	   :documentation "The parent transaction. Think of nested transactions semantics!!")))

(defclass stm-var ()
  ((name :initarg :name
	 :reader name
	 :required t :error-msg "Provide the variable name")
   (var :initarg :var
	:reader var
	:required t :error-msg "Provide the variable")
   ; (value :initarg :value :initform nil) Use some cached value??
   )
  (:metaclass required-slots-class))

(defmethod print-object ((stm-var stm-var) stream)
  (print-unreadable-object (stm-var stream :type t :identity t)
    (format stream "~A" (var stm-var))))

;; Errors of this kind should be raised whenever we want to alter an stm-transaction control flow.
;; That's why for example versioning-error should subclass from it

(define-condition stm-transaction-conflict-error (serious-condition)
  ())

(define-condition versioning-error (stm-transaction-conflict-error)
  ())

(defmethod initialize-instance :after ((stm-var stm-var) &rest initargs)
  "We create a ref if the client didn't provide one"
  (declare (ignore initargs))
  (let ((var (slot-value stm-var 'var)))
    (when (not (refp var))
      (setf (slot-value stm-var 'var) (make-instance 'ref
						 :name (name stm-var)
						 :value var)))))

(defmethod (setf value) (value (stm-var stm-var))
  "Sets the value of a transactional variable in the transaction.
If the transaction is nil, then we throw an error."
  (log-for stm "Setting value of: ~A to: ~A" stm-var value)
  (when (null (stm-transaction))
    (error 'no-transaction-error))
  (let* ((ref (var stm-var))
	 (old-value (value ref)))
    (setf (value (var stm-var)) value) ;; Change the value of the reference
    (add-change (stm-transaction) stm-var old-value value)))

(defmethod add-change ((stm-transaction stm-transaction) (stm-var stm-var) old-value new-value)
  ;; We add the change to the transaction if it was not already added
  (multiple-value-bind (_ foundp) (gethash stm-var (changes stm-transaction))
    (declare (ignore _))
    (when (not foundp)
      (setf (gethash stm-var (changes stm-transaction)) old-value))))

(defmethod value ((stm-var stm-var))
  (log-for stm "Reading value of: ~A" stm-var)
  (when (null (stm-transaction))
    (error 'no-transaction-error))
  (read-stm-var-value stm-var *stm-transaction*))

(defmethod read-stm-var-value ((stm-var stm-var) (stm-transaction stm-transaction))
  (let ((value (value (var stm-var))))
    (log-for stm "Reading value of: ~A from: ~A. Value: ~A" stm-var stm-transaction value)
    value))
  
(define-condition no-transaction-error (serious-condition)
  ()
  (:documentation "This error is thrown when we try to set a transactional variable value and there's no transaction running"))

(define-condition stm-var-not-found-error (serious-condition)
  ((stm-var :initarg :stm-var :reader stm-var))
  (:documentation "This error is thrown when the transaction doesnt have the passed transactional variable"))

(defmethod print-object ((err stm-var-not-found-error) stream)
  (print-unreadable-object (err stream :type t :identity t)
    (format stream "~A" (stm-var err))))

(defmethod stm-var-commit ((stm-var stm-var))
  "Commits the changes registered in the stm-var to the actual variable. We do nothing under optimistic semantics")

(defmethod stm-var-rollback ((stm-var stm-var) &optional (stm-transaction (stm-transaction)))
  (let
      ((transaction-value (gethash stm-var (changes stm-transaction))))
    (log-for stm "Rolling back ~A to ~A under ~A" stm-var stm-transaction transaction-value)
    (setf (value (var stm-var)) transaction-value)))

(defun stm-replace-forms (lexenv form replace-fn)
  "Takes into account lambda, let, let*, labels, flet, symbol-macrolet.
Ignores bindings in block, return-from, go, quote, throw"
  (when (or (keywordp form)
	  (constantp form))
    (return-from stm-replace-forms form))
  (if (symbolp form)
      ;; May be a free variable
      (if (member form lexenv)
	  ;; Lexical-variable
	  form
	  ;; Free-var
	  (funcall replace-fn form 'freevar))
      ;; It's not a var
      (case (car form)
	  (lambda (let ((args (list-lambda-list-vars (cadr form)))
		   (body (cddr form)))
	       (cons (car form)
		   (cons
		    (cadr form)
		    (mapcar (lambda (body-form)
			      (stm-replace-forms (append args lexenv)
				     body-form
				     replace-fn))
			    body)))))
	(let (let* ((new-lexenv '())
		    (bindings (mapcar (lambda (binding)
					(let ((new-binding (list (car binding)
								 (stm-replace-forms lexenv
										   (cadr binding)
										   replace-fn))))
					  (push (car binding) new-lexenv)
					new-binding))
				      (cadr form))))
	       (cons (car form)
		     (cons
		      bindings
		      (mapcar (lambda (body-form)
				(stm-replace-forms (append new-lexenv lexenv)
						 body-form
						 replace-fn))
			      (cddr form))))))
	(let* (let
		  ((bindings (mapcar (lambda (binding)
				      (list (car binding)
					    (stm-replace-forms lexenv
							      (cadr binding)
							      replace-fn))
				      (push (car binding) lexenv))
				     (cadr form))))
		(cons (car form)
		      (cons
		       bindings
		       (mapcar (lambda (body-form)
				 (stm-replace-forms lexenv body-form replace-fn))
			       (cddr form))))))
	(flet (let* ((new-lexenv '())
		    (bindings (mapcar (lambda (binding)
					(let* ((local-lexenv (append lexenv
								     (list-lambda-list-vars (cadr binding))))
					       (new-binding (cons (car binding)
								 (cons (cadr binding)
								       (mapcar (lambda (body-form)
										 (stm-replace-forms
										  local-lexenv
										  body-form
										  replace-fn))
									       (cddr binding))))))
					  (push (car binding) new-lexenv)
					  new-binding))
				      (cadr form))))
		(cons (car form)
		      (cons
		       bindings
		       (mapcar (lambda (body-form)
				 (stm-replace-forms (append new-lexenv lexenv)
						   body-form
						   replace-fn))
			       (cddr form))))))
	  (labels (let* ((new-lexenv '())
			 (bindings (mapcar (lambda (binding)
					   (let* ((local-lexenv (append lexenv
									(list-lambda-list-vars (cadr binding))
									(list (car binding)))))
					     (push (car binding) new-lexenv)
					     (cons (car binding)
						   (cons (cadr binding)
							 (mapcar (lambda (body-form)
								   (stm-replace-forms
								    (append local-lexenv new-lexenv)
								    body-form
								    replace-fn))
								 (cddr binding))))))
					   (cadr form))))
		    (cons (car form)
			  (cons
			   bindings
			   (mapcar (lambda (body-form)
				     (stm-replace-forms (append lexenv new-lexenv)
						       body-form replace-fn))
				   (cddr form))))))
	;; (symbol-macrolet) I assume symbol-macrolet gets processed previously
	  (quote form)
	  (return-from (list (car form) (cadr form)
			     (stm-replace-forms lexenv (caddr form) replace-fn)))
	  (declare form)
	  (t (funcall replace-fn form 'apply
		      (lambda (args)
			(mapcar (lambda (arg)
				  (stm-replace-forms
				   lexenv
				   arg
				   replace-fn)) args)))))))

(defvar *stmfuns* (make-hash-table :test #'equal))
  
(defmacro stm (&rest body)
  (let ((new-body (loop for body-form in body
		     collect
		       (stm-replace-forms '() body-form
					  (lambda (form type &rest args)
					    (let ((cont (car args)))
					      (ecase type
						(freevar `(value ,form))
						(apply
						 (if (gethash (car form) *stmfuns*)
						     form
						     ;; else
						     (cons (car form) (funcall cont (cdr form))))))))))))
    `(progn ,@new-body)))
    
(defmacro defunstm (name args &rest body)
  `(progn
     (setf (gethash ',name *stmfuns*) t)
     (defun ,name ,args (stm ,@body))))

(defmacro atomically (&body body)
  (with-unique-names (atomic-block)
    `(unwind-protect
	  (tagbody ,atomic-block
	     (restart-case (progn
			     (begin-stm-transaction)
			     ,@body
			     (commit-stm-transaction))
	       (retry-transaction ()
		 :report (lambda (stream)
			   (format stream "Restart the memory transaction"))
		 ;; This is all. It's responsibility of the piece of code that
		 ;; throws errors to provide other restarts, such as :continue, for example.
		 (rollback-stm-transaction)
		 (go ,atomic-block))
	       (abort-transaction ()
		 :report (lambda (stream)
			   (format stream "Abort the memory transaction"))
		 (rollback-stm-transaction))))
       (when (stm-transaction)
	 (rollback-stm-transaction)))))


(defun begin-stm-transaction ()
  (setf *stm-transaction* (make-instance 'stm-transaction
					   :parent *stm-transaction*))
  (log-for stm "Beggining transaction: ~A" *stm-transaction*))

(defun commit-stm-transaction (&optional (stm-transaction (stm-transaction)))
  (log-for stm "Commiting transaction: ~A" stm-transaction)
  (setf *stm-transaction* (parent stm-transaction)))

(defun rollback-stm-transaction (&optional (stm-transaction (stm-transaction)))
  (log-for stm "Rolling back transaction: ~A" stm-transaction)
  ;; Rollback each of the transaction variables
  (maphash (lambda (stm-var old-value)
	     ;(stm-var-rollback stm-var stm-transaction)
	     (log-for stm "Rolling back ~A to ~A" stm-var old-value)
	     (setf (value stm-var) old-value))
	   (changes stm-transaction))
  ;; Discard the transaction
  (setf *stm-transaction* (parent *stm-transaction*)))

(defmethod (setf *stm-transaction*) (stm-transaction)
  (setq *stm-transaction* stm-transaction)
  (log-for stm "Current transaction: ~A" *stm-transaction*))

(make-let stm-vars (lambda (var-name ref)
		     `(make-instance 'stm-var
				     :name ,(symbol-name var-name)
				     :var ,ref)))

(make-using stm-vars (lambda (binding)
		      `(value ,binding)))

(make-with stm-vars)

(defmacro with-bindings-as-stm-vars (bindings &body body)
  `(with-stm-vars ,(loop for binding in bindings
			collect (list binding binding))
     ,@body))