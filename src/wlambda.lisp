;; wlambdas are lambdas bound weakly to some variables
;; useful for setting as listeners

(declaim (optimize (debug 3) (speed 0)))
(require :log5)
(require :cl-who)
(require :parenscript)

(defpackage dataflow
  (:use :common-lisp :sb-ext :sb-mop :log5 :cl-who :parenscript)
  (:nicknames :df)
  (:export
   #:wlambda
   #:dfcell
   #:dfvaluecell
   #:value
   #:test
   #:add-listener
   #:df
   #:df-lambda
   #:trigger-event
   #:run-listener
   #:with
   #:dataflow-class
   #:with-df-slots
   #:*track*))

(in-package :df)

;; log5 config
(defcategory mop)
(defcategory df)
(defcategory all
    (or mop df))
(defcategory none)

#|
(start-sender 'mop
  (stream-sender :location *error-output*)
  :category-spec '(mop)
  :output-spec '(message))

(start-sender 'df
  (stream-sender :location *error-output*)
  :category-spec '(df)
  :output-spec '(message))

(start-sender 'all
  (stream-sender :location *error-output*)
  :category-spec '(all)
  :output-spec '(message))

(start-sender 'none
  (stream-sender :location *error-output*)
  :category-spec '(none)
  :output-spec '(message))



|#

(defvar *debug* t "Turn off to disable debugging macros. Note you'll have to recompile your code in order for this to be effective")

(defvar *track* nil "Turn on if you want to track the dataflow")

(defmacro defdbgmacro (name args &rest body)
  `(defmacro ,name ,args
     (when *debug*
	 ,@body)))

(defdbgmacro trk (datum &rest args)
  `(when *track*
     (format t ,datum ,@args)))

(defun make-weakref (object)
  #+sbcl(make-weak-pointer object)
  #-sbcl(error "make-weakref not implemented on this lisp"))

(defun weakref-value (weakref)
  #+sbcl(weak-pointer-value weakref)
  #-sbcl(error "weakref-value not implemented on this lisp"))

(defun list-lambda-list-vars (lambda-list)
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
				 morep more-context more-count)
      (sb-int:parse-lambda-list lambda-list)
    (declare (ignore keyp allowp auxp aux morep more-context more-count))
  (append
   required
   (mapcar (lambda (pair) (car pair)) optional)
   (mapcar (lambda (maybe-pair)
	     (if (consp maybe-pair)
		 (car maybe-pair)
		 maybe-pair)) keys)
   (when restp
     (list rest)))))


(defun replace-freevars (lexenv form replace-fn &optional (skip-form nil))
  "Takes into account lambda, let, let*, labels, flet, symbol-macrolet.
Ignores bindings in block, return-from, go, quote, throw"
  (when (or (keywordp form)
	  (constantp form))
    (return-from replace-freevars form))
  (if (symbolp form)
      ;; May be a free variable
      (if (member form lexenv)
	  ;; Lexical-variable
	  form
	  ;; Free-var
	  (funcall replace-fn form))
      ;; It's not a var
      (progn
	(when (and (not (null skip-form)) (equal (cadr form) skip-form))
	  (return-from replace-freevars form))
	(case (car form)
	  (lambda (let ((args (list-lambda-list-vars (cadr form)))
		   (body (cddr form)))
	       (cons (car form)
		   (cons
		    (cadr form)
		    (mapcar (lambda (body-form)
			      (replace-freevars (append args lexenv)
				     body-form
				     replace-fn))
			    body)))))
	(let (let* ((new-lexenv '())
		    (bindings (mapcar (lambda (binding)
					(let ((new-binding (list (car binding)
								 (replace-freevars lexenv
										   (cadr binding)
										   replace-fn))))
					  (push (car binding) new-lexenv)
					new-binding))
				      (cadr form))))
	       (cons (car form)
		     (cons
		      bindings
		      (mapcar (lambda (body-form)
				(replace-freevars (append new-lexenv lexenv)
						 body-form
						 replace-fn))
			      (cddr form))))))
	(let* (let*
		  ((new-lexenv lexenv)
		   (bindings (mapcar (lambda (binding)
				       (let ((new-binding
					      (list (car binding)
						    (replace-freevars new-lexenv
								      (cadr binding)
								      replace-fn))))
					 (push (car binding) new-lexenv)
					 new-binding))
				     (cadr form))))
		(cons (car form)
		      (cons
		       bindings
		       (mapcar (lambda (body-form)
				 (replace-freevars new-lexenv body-form replace-fn))
			       (cddr form))))))
	(flet (let* ((new-lexenv '())
		    (bindings (mapcar (lambda (binding)
					(let* ((local-lexenv (append lexenv
								     (list-lambda-list-vars (cadr binding))))
					       (new-binding (cons (car binding)
								 (cons (cadr binding)
								       (mapcar (lambda (body-form)
										 (replace-freevars
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
				 (replace-freevars (append new-lexenv lexenv)
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
								   (replace-freevars
								    (append local-lexenv new-lexenv)
								    body-form
								    replace-fn))
								 (cddr binding))))))
					   (cadr form))))
		    (cons (car form)
			  (cons
			   bindings
			   (mapcar (lambda (body-form)
				     (replace-freevars (append lexenv new-lexenv)
						       body-form replace-fn))
				   (cddr form))))))
	;; (symbol-macrolet) I assume symbol-macrolet gets processed previously
	  (quote form)
	  (return-from (list (car form) (cadr form)
			     (replace-freevars lexenv (caddr form) replace-fn)))
	  (declare form)
	  (t (cons (car form) (mapcar (lambda (arg) (replace-freevars
						lexenv
						arg
						replace-fn)) (cdr form))))))))

#|
Tests:

(replace-freevars '() '(print "hola") (lambda (x) x))
(replace-freevars '() '(let ((x 2) (y x)) (+ 1 2) (print "hola" x) 'hoa) (lambda (x) 2))
(replace-freevars '() '(let* ((x  (print "hola")) (y (adsf)) (z 34)) (+ 1 2) (print "hola" x) 'hoa) (lambda (x) 2))
(replace-freevars '() '(lambda (x &optional (y 4)) (print x) (print y)) (lambda (x) 2))
(replace-freevars '() '(flet ((my-f () (print x) my-f) (other-f (x) my-f x)) my-f other-f) (lambda (x) 2))
(replace-freevars '() '(labels ((my-f () (print x) my-f) (other-f (x) my-f x)) my-f other-f) (lambda (x) 2))

|#

(defun list-free-vars (body)
  (let
      ((freevars (make-hash-table :test #'equal))
       (freevars-list '()))
    (mapcar (lambda (body-form)
	      (replace-freevars '()
				body-form
				(lambda (freevar)
				  (multiple-value-bind (previous-lexvar found)
				      (gethash freevar freevars)
				    (declare (ignore previous-lexvar))
				    (when (not found)
				      (setf (gethash freevar freevars) freevar))))))
	    body)
    (maphash (lambda (freevar value)
	       (declare (ignore value))
	       (push freevar freevars-list))
	     freevars)
    freevars-list))


(defun list-free-vars-non-external (body)
  (let
      ((freevars (make-hash-table :test #'equal))
       (freevars-list '())
       (external-vars
	(let ((maybe-declare (car body)))
	  (if (and (equal (string (car maybe-declare)) "DECLARE")
		   (equal (string (caadr maybe-declare)) "EXTERNAL"))
	      (cdadr maybe-declare)
	      '()))))
    (mapcar (lambda (body-form)
	      (replace-freevars '()
				body-form
				(lambda (freevar)
				  (when (not (member freevar external-vars))
				    (multiple-value-bind (previous-lexvar found)
					(gethash freevar freevars)
				      (declare (ignore previous-lexvar))
				      (when (not found)
					(setf (gethash freevar freevars) freevar)))))))
	    body)
    (maphash (lambda (freevar value)
	       (declare (ignore value))
	       (push freevar freevars-list))
	     freevars)
    freevars-list))

(defun prune-externals (code)
  (let ((externals '())
	(non-external-declarations '()))
    (flet ((declaration-p (sentence)
	     (equal (string (car sentence)) "DECLARE"))
	   (declaration (declaration)
	     (string (caadr declaration))))
      (loop for maybe-declaration = (car code)
	 while (declaration-p maybe-declaration)
	 do
	   (progn
	     (setf code (cdr code))
	     (if (equal (declaration maybe-declaration) "EXTERNAL")
	       (setf externals (append (cdadr maybe-declaration) externals))
	       (push maybe-declaration non-external-declarations)
	       ))))
    (values code non-external-declarations externals)))


(defmacro wlambda (args &rest body)
  (let*
      ((freevars (make-hash-table :test #'equal))
       (table (make-hash-table :test #'equal))
       (body (macroexpand body)))
    (multiple-value-bind (body non-external-declarations external-vars)
	(prune-externals body)
      (let
	  ((new-body (mapcar (lambda (body-form)
			       (replace-freevars (list-lambda-list-vars args)
						 body-form
						 (lambda (freevar)
						   (if (not (member freevar external-vars))
						       (multiple-value-bind (previous-lexvar found)
							   (gethash freevar freevars)
							 (if found
							     previous-lexvar
							     (let ((gen-lexvar (gensym (string freevar))))
							       (setf (gethash freevar freevars) gen-lexvar)
							       gen-lexvar)))
						       freevar))))
			     body)))
              (let ((freevars-list '()))
		(maphash (lambda (freevar gen-lexvar)
			   (push (cons freevar gen-lexvar) freevars-list))
			 freevars)
		`(let ,(mapcar (lambda (fv)
				 (let* ((freevar (car fv))
					(sym (gensym (string freevar))))
				   (setf (gethash freevar table) sym)
				   (list sym `(make-weakref ,freevar))))
			       freevars-list)
		   (lambda ,args
		     ,@non-external-declarations
		     (let ,(mapcar (lambda (fv)
			  (let ((gen-lexvar (cdr fv)))
			    (list gen-lexvar nil)))
			  freevars-list)
		       (when (and ,@(mapcar (lambda (fv)
					      (let ((freevar (car fv))
						    (weakref (cdr fv))
						    (target (gensym "TARGET"))
						    (found (gensym "FOUND")))
						`(multiple-value-bind (,target ,found)
						     (weakref-value ,(gethash freevar table))
						   (setf ,weakref ,target)
						   ,found)))
					    freevars-list))
			 ,@new-body)))))))))
#| Example:
(wlambda (button)
	 (declare (ignore button))
	 (declare (external those))
	 (setf (color button) 'black)
	 (set-color this 'red)
	 (setf (color that) 'blue)
	 (print (message this))
	 (print those))

|#


(defmacro dflambda (args &rest body)
  (let*
      ((freevars (make-hash-table :test #'equal))
       (table (make-hash-table :test #'equal))
       (body (macroexpand body)))
    (multiple-value-bind (body non-external-declarations external-vars)
	(prune-externals body)
      (let
	  ((new-body (mapcar (lambda (body-form)
			  (replace-freevars (list-lambda-list-vars args)
					    body-form
					    (lambda (freevar)
					      (if (not (member freevar external-vars))
						  (multiple-value-bind (previous-lexvar found)
						      (gethash freevar freevars)
						    (if found
							`(progn
							   (log-for df "Getting value of df var: ~A~%" ,previous-lexvar)
							   (value ,previous-lexvar))
							(let ((gen-lexvar (gensym (string freevar))))
							  (setf (gethash freevar freevars) gen-lexvar)
							  `(progn
							     (log-for df "Getting value of df var: ~A~%" ,gen-lexvar)
							     (value ,gen-lexvar)
							     ))))
						  ;; else
						  freevar
					))))
			body)))
	(let ((freevars-list '()))
	  (maphash (lambda (freevar gen-lexvar)
		     (push (cons freevar gen-lexvar) freevars-list))
		   freevars)
	  `(let ,(mapcar (lambda (fv)                          ;; weakref vars
			   (let* ((freevar (car fv))
				  (sym (gensym (string freevar))))
			     (setf (gethash freevar table) sym)
			     (list sym `(make-weakref ,freevar))))
			 freevars-list)
	     (lambda ,args
	       ,@non-external-declarations
	       (let ,(mapcar (lambda (fv)                       ;; local vars referencing wekref vars
			       (let ((gen-lexvar (cdr fv)))
				 (list gen-lexvar nil)))
			     freevars-list)
		 (when (and ,@(mapcar (lambda (fv)           ;; weakvars checking before executing the body
					(let* ((freevar (car fv))
					       (weakref (cdr fv))
					       (target (gensym "TARGET"))
					       (found (gensym "FOUND")))
					  `(multiple-value-bind (,target ,found)
					       (weakref-value ,(gethash freevar table))
					     (if ,found
						 (progn
						   (setf ,weakref ,target)
						   t)
					;else
						 (progn
						   (log-for df "Broken weak-reference ~A" ,(gethash freevar table) )
						   nil)
						 ))))
				      freevars-list))
		   ,@new-body)))))))))


(defclass dfcell ()
  ((listeners :initform (make-hash-table :test #'equal))
   (code)))

(defclass dfvaluecell (dfcell)
  ((value :accessor value :initarg :value)
   (test :accessor test :initarg :test :initform #'eq)))

(defmethod print-object ((dfcell dfvaluecell) stream)
  (print-unreadable-object (dfcell stream :type t :identity t)
    (format stream "value: ~A test: ~A"
	    (if (slot-boundp dfcell 'value)
		(value dfcell)
		"#unbound")
	    (test dfcell))
  ))

(defgeneric run-listener (listener event triggerer &optional args))

(defmethod run-listener ((listener function) event triggerer &optional args)
  (log-for df "Running listener: ~A event: ~A triggerer: ~A~%" listener event triggerer)
  (funcall listener event triggerer args))

(defmethod run-listener ((listener t) event triggerer &optional args)
  (declare (ignore event triggerer args))
  (error "run-listener not declared for listeners of type ~A~%" (type-of listener)))

(defun trigger-event (event triggerer &rest args)
  (log-for df "Event ~A triggered on ~A args: ~A~%" event triggerer args)
  (let ((event-table (gethash event (slot-value triggerer 'listeners))))
    (when event-table
      (maphash (lambda (listener value)
	       (declare (ignore value))
	       (run-listener listener event triggerer args))
	       event-table))))

(defmethod (setf value) (new-value (dfcell dfvaluecell))
  (when (or (not (slot-boundp dfcell 'value))
	    (not (funcall (test dfcell) new-value (value dfcell))))
    (log-for df "Setting value of: ~A  to: ~A~%" dfcell new-value)
    (setf (slot-value dfcell 'value) new-value)
    (trigger-event :changed dfcell new-value)))


(defmethod add-listener (listener target &optional (event :changed))
  (log-for df "Adding listener: ~A  to: ~A for event: ~A~%" listener target event)
  (let ((events-table
	 (progn
	   (when (not (gethash event (slot-value target 'listeners)))
	     (setf (gethash event (slot-value target 'listeners))
		   (make-hash-table :test #'eq)))
	   (gethash event (slot-value target 'listeners)))))
    (setf (gethash listener events-table) listener))
  ;; We run the listener to initialize its values
  (when (equal event :changed)
    (run-listener listener event target))
  listener)

(defclass growable-hash-table ()
  ((test :initarg :test :initform #'eql :accessor test)
   (grow-table :initarg :grow-table :accessor grow-table)
   (table :accessor table)

   ))

(defmethod initialize-instance :after ((ght growable-hash-table) &rest initargs)
  (declare (ignore initargs))
  (setf (table ght) (make-hash-table :test (test ght)))
  (setf (grow-table ght) (lambda () (make-hash-table :test (test ght)))))

(defun make-growable-hash-table (&optional (test #'eql))
  (make-instance 'growable-hash-table :test test))

(defmethod get-hash (key1 key2 (ghash-table growable-hash-table))
  (symbol-macrolet
      ((tb (gethash key1 (table ghash-table))))
    (when tb
      (gethash key2 tb))))

(defmethod (setf get-hash) (value key1 key2 (ght growable-hash-table))
  (symbol-macrolet
      ((tb (gethash key1 (table ght))))
    (when (not tb)
      (setf tb (funcall (grow-table ght))))
    (setf (gethash key2 tb) value)))


(defmacro df (&rest body)
  (let* ((dffcell (gensym "DFCELL"))
	(listener-f (gensym "LISTENER-F"))
	(value (gensym "VALUE"))
	(event (gensym "EVENT"))
	(triggerer (gensym "TRIGGERER"))
	(new-value (gensym "NEW-VALUE"))
	(externals (if (and (equal (string (caar body)) "DECLARE") (equal (string (caadar body)) "EXTERNAL"))
		     (cdadar body) nil))
	 (new-body (if externals
		       (cdr body)
		       body)))
    `(let* ((,dffcell (make-instance 'dfvaluecell))
	    (,listener-f
	    (dflambda (,event ,triggerer ,new-value)
		      (declare (external ,@(cons dffcell externals)))
		      (declare (ignore ,event ,triggerer ,new-value))
		      (let ((,value (progn ,@new-body)))
			(setf (value ,dffcell) ,value)
			,value))))
       (setf (slot-value ,dffcell 'code) '(dflambda (,event ,triggerer ,new-value)
					   (declare (external ,@(cons dffcell externals)))
					   ,@new-body))
       ,@(loop for freevar in (list-free-vars-non-external (cons `(declare (external ,@(cons dffcell externals))) body))
	    collect `(add-listener ,listener-f ,freevar :changed))
       ,dffcell)))
#|
Tests:

(setf *track* t)

(defclass bank-account ()
  ((money :initform (make-instance 'dfvaluecell :value 0))))

(defparameter *ba* (make-instance 'bank-account))

;; IMPORTANT: note that in *r*, the reference to b is lost, so once
;; the gc is run, *r* is not updated anymore
;; Play with (gc :full t) to see that.
;; The correct version is *w*

(defparameter *r*
  (let ((suma (let ((slot (slot-value *ba* 'money))
		    (b (make-instance 'dfvaluecell :value 2)))
		(df
		 (+ slot b)))))
    (df
     (format t "Getting value of suma1!!~%")
     (* suma 5))))

(defparameter *w*
  (let ((suma (let ((slot (slot-value *ba* 'money))
		    (b 2))
		(df
		 (declare (external b))
		 (+ slot b)))))
   (df
     (format t "Getting value of suma1!!~%")
     (* suma 5))))

(defparameter *s*
  (let ((slot (slot-value *ba* 'money)))
    (df
     (let*
	 ((value 44)
	  (suma (+ slot value)))
       (format t "Getting value of suma2!!~%")
       (* suma value)))))

(setf (value (slot-value *ba* 'money)) 2)
(setf (value (slot-value *ba* 'money)) 10)

|#

#|

Nota: en este momento el algoritmo de dataflow se encuentra
en los wlambda creados. Para desacoplar el algoritmo habria que transformar el codigo de forma tal de crear lambdas que bindeen con nuevas variables todas las variables libres. Además, cada celda debería contener una tabla weak a las variables libres. Para ejecutar, habria que pasar esas variables al nuevo lambda generado.

|#

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


#|

Test:

(defunstm stmtest ()
  (print y))

 (stm
 (print x)
 (let ((x y))
   (print x))
 (stmtest w)
 )
|#

;; MOP glue for dataflow


(defclass dataflow-class (standard-class)
  ((dataflow-slots :initform '()
		   :accessor dataflow-slots)))

(defclass dataflow-object ()
  ((dataflow-slots :initform (make-hash-table :test #'equal)
		   :accessor dataflow-slots)
   (listeners :initform (make-hash-table :test #'equal) :accessor listeners))

  )


(defmethod validate-superclass ((class standard-class) (super dataflow-class))
  t)

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
  (log-for mop "Leaving: dataflow-class shared-initialize :around~%")
  )

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

;; Example with the new MOP glue

#|
Tests:

(setf *track* t)

(defclass b-account ()
  ((money :initform 0 :dataflow t))
  (:metaclass dataflow-class))

(defparameter *ba* (make-instance 'b-account))

(defparameter *w*
  (let ((suma (let ((b 2))
		(with-df-slots (money) *ba*
			       (df
				(declare (external b))
				(+ money b))))))
    (df
     (format t "Getting value of suma1!!~%")
     (* suma 5))))

(defparameter *s*
  (with-df-slots (money) *ba*
    (df
     (let*
	 ((value 44)
	  (suma (+ money value)))
       (format t "Getting value of suma2!!~%")
       (* suma value)))))

(setf (slot-value *ba* 'money) 2)
(format t "Money:~A~%" (slot-value *ba* 'money))

(with-slots (money) *ba*
  (setf money 3))

|#

#|
Now we apply free-variables detection to build a javascript-server comunication library

(defcomponent my-object-deleter ()
  ()
  (:initialize ()
	       (let ((object (model self)))
		 (add-action-link :name "delete-object"
				  :default-display "Delete object"
				  :action
				  (client
				   ;; The free variable "object" is passed to the client
				   (if (open-dialog 'question-dialog :text (format nil "Are you sure you want to delete ~A?" object))
				       (server
					;; Now the free variable "object" refers to the client proxy. The client passes de object id
					(delete-object object))))
				)
	       )
  ))

;; Different identifyable lambdas should be created in both sides, in the client and the server. We assign an id to each of them and call them distributely (RPC)

;; For example, for the above example, we have two lambdas:

  In the client:
  (create-lambda (object)
		 (if (open-dialog 'question-dialog :text (format nil "Are you sure you want to delete ~A?" object))
		     (call-server :session 4 :id 44 :params object)))

  In the server:
  (create-lambda (object)
		 (delete-object object))

  And the resulting action is:
  (let ((object (model self)))
		 (add-action-link :name "delete-object"
				  :default-display "Delete object"
				  :action (call-client :session *session* :id 33 :params object)))

  Besides, the javascript to create lambdas is dynamically transferred when the component is active

  |#

(defclass component ()
  ((children :accessor children :initform '()))
  )

(defclass widget ()
  ()
  )

(defclass action-link (widget)
  ((name :accessor name :initarg :name)
   (default-display :accessor default-display :initarg :default-display)
   (action :accessor action :initarg :action)))

(defvar *server-entry-counter* 1)
(defvar *server-entries* (make-hash-table :test #'equal))

(defvar *client-entry-counter* 1)
(defvar *client-entries* (make-hash-table :test #'equal))


(defun register-server-entrypoint (exp)
  (let ((entry-id *server-entry-counter*)
	(freevars (list-free-vars (list exp))))
    (incf *server-entry-counter*)
    (values
     entry-id
     freevars
    `(setf (gethash ,entry-id *server-entries*)
	  (lambda ,freevars ,exp)))))

(defun register-client-entrypoint (exp)
  (let ((entry-id *client-entry-counter*)
	(freevars (list-free-vars (list exp))))
    (incf *client-entry-counter*)
    (values
     entry-id freevars
     `(setf (gethash ,entry-id *client-entries*)
				     (lambda ,freevars, exp)))))

(defvar *in-server-context* t)
(defvar *client-lambdas* '())
(defvar *server-lambdas* '())

(defun process-action (code)
  (labels
	((process-tree (exp)
	   (if (atom exp)
	       exp
	       (let ((operation (car exp))
		     (args (cdr exp)))
		 (case operation
		   (server
		     (if *in-server-context*
			 `(progn
			    ,@(mapcar #'process-tree args))
			 (call-server `(progn ,@args))))
		   (client
		    (if *in-server-context*
			(call-client `(progn ,@args))
			`(progn
			   ,@(mapcar #'process-tree args))))
		   (let
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(let
			      ,(loop for binding in bindings
				  for form = (cadr binding)
				  collect
				    (list (car binding) (process-tree form)))
			    ,(process-tree `(progn ,@body)))))
		   (let*
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(let*
			     ,(loop for binding in bindings
				 for form = (cadr binding)
				 collect (list (car binding) (process-tree form)))
			 ,(process-tree `(progn ,@body)))))
		   (flet
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(flet
			      ,(loop for binding in bindings
				  for fbody = (nth 2 binding)
				  collect (list (car binding) (cadr binding) (process-tree `(progn ,@fbody))))
			    ,(process-tree `(progn ,@body)))))
		   (labels
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(labels
			      ,(loop for binding in bindings
				  for fbody = (nth 2 binding)
				  collect (list (car binding) (cadr binding) (process-tree `(progn ,@fbody))))
			    ,(process-tree `(progn ,@body)))))
		   (return-from
		    (error "Unimplemented"))
		   (t
		    `(,operation ,@(mapcar #'process-tree args)))))))
	 (call-server (code)
	   (let ((*in-server-context* t))
	     ;; Register a server-entry-point from the code
	     (multiple-value-bind (id parameters lambdas-code)
		 (register-server-entrypoint (process-tree code))
	       (push lambdas-code *server-lambdas*)
	       ;; Return the code that calls it
	       `(server-call ,id ,@parameters))))
	 (call-client (code)
	   (let ((*in-server-context* nil))
	     (multiple-value-bind (id parameters lambdas-code)
		 (register-client-entrypoint (process-tree code))
	       (push lambdas-code *client-lambdas*)
	       ;; Return the code that calls it
	       `(client-call ,id ,@parameters)))))
	 (process-tree code)))

(defun process-action-2 (code)
  (let
      ((*in-server-context* nil)
       (*client-lambdas* '())
       (*server-lambdas* '())
       (*client-entry-counter* 1)
       (*server-entry-counter* 1))
    (let ((action-code (process-action code)))
      (values action-code
	      `(progn ,@*server-lambdas*)
	      (compile-script `(progn ,@*client-lambdas*))))))

;; Test

;; (process-action-2 '(progn (let ((chau "chau")) (client (print "hola" obj) (server (print "hola" chau) (client (print "hi")))))))

(defmacro add-action-link (c &key name default-display action)
  (multiple-value-bind
	(action-code server-lambdas client-lambdas)
      (process-action-2 (macroexpand action))
    `(progn
       (push (make-instance 'action-link :name ,name :default-display ,default-display :action (lambda () ,(compile-script action-code))) (children ,c))
       ,server-lambdas
       ,client-lambdas)))

#| Test:


(add-action-link my-component
		 :name "my-component"
		 :default-display "My Component"
		 :action (progn (print "hola" obj) (server (let ((chau "chau")) (print "hola" chau) (client (print "hi") (print chau))))))


Problema con esta version de add-action-link: es estrictamente necesario hacer una conversión CPS por cada operacion server y client. Esto es así ya que, en el caso de un rendering standard, el código a continuar debe ir en un atributo onLoad del tag body. En el caso de Ajax, el codigo a continuar debe ir de forma similar atacheado a un objeto HttpRequest en forma de callback. Una particularidad de esta conversión es que debe tener en cuenta las variables libres para saber exactamente que variables vamos pasando del servidor al cliente y del cliente al servidor (no tenemos ni un stack reificado (interprete) ni un stack implícito con bindings en memoria (lambdas + frames))

|#



;; Thought. The MOP provides similar advantages than monads. A uniform interface.
;; If we implement metaclasses with method combinations, then we may be able to combine two or more
;; metaclasses plugging them through inheritance. It may depend on how the metacode was implemented, though.

;; Example:

(defclass persistent-dataflow-class (persistent-class dataflow-class)
  ()
  )

(defmacro ps+ (output-stream &body body)
  (compile-script `(progn ,@body) :output-stream output-stream))

(defmacro with-html  (&body body)
  (let ((s (gensym "HTML-STREAM-")))
    `(let ((,s (make-string-output-stream)))
       (with-html-output-to-string (,s)
	 ,@body))))

(defpsmacro js (&body)
  `(str (ps ,@body))
  )

(with-html
    (:html
     (:head
      (:title "Test page"))
     (:script
      (str
      (ps
	 ;; We can get rid of this hash-tables if we generate names
	 ;; and hard-core bind them from the lambdas definitions
	 ;(defvar *client-lambdas* (make-hash-table :test #'equal))
	 ; The following are hardcoded lambdas :)
	 (defun client-lambda-1 ()
	   (alert "hello!!")
	   (call-server *session-id* "server-lambda-1")
	   )
#|
	  Now we have to think about proxy generation. server-object is a proxy object. Although we can do certain things with it on the client, like printing it and get its slots *values* (numbers, and strings). If we want one of his collaborators, we have to declare it on the server code and use it on the client like this:
	  (server
	 (let ((friend (slot-value person 'friend)))
	   (client
	       (print person)
	       (print friend))))
	 |#

	 (defun client-lambda-2 (server-proxy)
	   (alert "Hello again!!")
	   (alert (print server-proxy))
	   (call-server *session-id* "server-lambda-2" "This is a message from the client"))))))




     (:body :on-load "client-lambda-1()" ;; The function to on load is updated with each response from the server. In case of AJAX, we need a XML Command to perform the dispatch

      (:p "Action test page")))



;; NUEVA IDEA:
;; PROBLEMA: las continuaciones compiladas y el backtracking no se llevan bien. Esto es así ya que no es posible determinar cuales objetos a copiar, ni como. Además, las continuaciones compiladas en lambdas ya contienen los bindings a los componentes; no es posible cambiar estos bindings para que apunten a copias de ellos. Bajo un interprete, esto no es problema ya que los registros de activación están reificados; por lo tanto, es posible implementar continuaciones copiando estos registros de activación y los componentes y objetos a los que se apunta; no es necesario ninguna transformación de código. Sin embargo, es posible tener continuaciones compiladas y backtracking, al mismo tiempo, si podemos implementar las estructuras a backtrackear como estructuras de datos persistentes, más un protocolo de que partes se persisten y como a determinar por el usuario. Además, el esquema de estructuras persistentes es independiente de las continuaciones, por lo que el control de flujo puede implementarse de cualquier otra forma y seguir teniendo backtracking. Este diseño está mucho menos acoplado que el del uso de continuaciones y copia de registros de activacion.



;; Ahora veamos la implementación de nuestra estructura de datos persistente. Utilizaremos la técnica path-copy. Cada vez que se hace un cambio al árbol de componentes, este copia mediante una implementación path-copy. Los nodos son copiados mediante una funcion genérica: copy-node. Esta función puede especializarse para copiar los objetos del modelo pertinentes en caso de backtracking. De ahora en más, cada requerimiento deberá contener la versión del árbol de componentes que se va a utilizar. Como a priori no sabemos aquellas versiones que serán utilizadas (el usuario podría entrar cualquier url en un momento determinado), no es posible valernos solo de enlaces a los roots de las versiones y que estas sean garbage collectadas cuando no se requieran mas; es necesario indexar las versiones en otra estructura de datos para hacerlas permanentes.

(defclass node-mixin ()
  ((children :initform '() :accessor children :initarg :children)))

(defclass with-parent-node-mixin (node-mixin)
  ((parent :initform nil :accessor parent)))

(defclass value-node-mixin (node-mixin)
  ((value :accessor value :initarg :value)))

(defun not-rootp (node)
  (null (parent node)))


(defun gfoldl (next null data f a)
  "A foldl on generic datastrutures.
     next: the function to traverse the datastructure. Should give the next element.
     null: the function to tell if we have got to the base case.
     data: the data structure.
     f: the processing function. Takes the current node and the processed structure. Should give a new processed structure.
     a: the accumulated processed structure."
  (if (funcall null data)
      a
      (gfoldl next null
	      (funcall next data) f
	      (funcall f data a))))

;; Example:
(defun list-foldl (list f a)
  (gfoldl #'cdr #'null list f a))

(defun parent-path-foldl (node f a)
  (gfoldl #'parent #'not-rootp node f a))

(defun parent-key-path-foldl (node f a &optional (child-key nil))
  (if (not-rootp node)
      a
      (parent-key-path-foldl (parent node)
			 f
			 (funcall f node a child-key)
			 (child-key node (parent node)))))

(defgeneric copy-node (node-mixin))

(defmethod copy-node ((node node-mixin))
  (let ((new-node (allocate-instance (class-of node))))
    (setf (children new-node) (children node)
	  (parent new-node) (parent node))
    new-node))

(defmethod copy-node :around ((node value-node-mixin))
  (let
      ((new-node (call-next-method)))
    (setf (value new-node) (value node))
    new-node))

;; node-children-indexing-mixins
(defclass node-children-indexing-mixin () ())
(defclass hash-node-children-indexing-mixin (node-children-indexing-mixin)
  ())

(defmethod initialize-instance ((node hash-node-children-indexing-mixin) &rest init-args)
  (call-next-method)
  (setf (children node) (make-hash-table))
  (let ((children-list (getf init-args :children)))
    (loop for cons-or-child in children-list
       do (if (consp cons-or-child)
		(set-child node (car cons-or-child) (cdr cons-or-child))
		(add-child node cons-or-child)))))


(defgeneric set-child (node-children-indexing-mixin place child)
  (:documentation "sets a node child depending on the implementation"))

(defmethod set-child ((node hash-node-children-indexing-mixin) place child)
  (setf (gethash place (children node)) child)
  (setf (parent child) node))

(defmethod add-child ((node hash-node-children-indexing-mixin) child)
  (set-child node child child))

(defun copy-tree-from-node (node)
  (parent-key-path-foldl node
		     (lambda (parent new-child child-key)
		       (set-child (copy-node parent) child-key new-child))
		     nil))

(defun replace-node (node new-node)
  (parent-key-path-foldl (parent node)
		     (lambda (parent new-child child-key)
		       (set-child (copy-node parent) child-key new-child))
		     new-node))

(defun child-key (child parent)
  (if (eql (gethash child (children parent)) child)
      child
      (maphash (lambda (k v) (when (eql v child) (return-from child-key k))) (children parent)))
  (error "Child not found ~A in ~A" child parent))

(defmethod print-tree ((node value-node-mixin) &optional (stream *standard-output*))
  (format stream "[~A " (value node))
  ; Important: we assume the node indexing comes after the node type in the hierarchy. The following (call-next-method) is supposed to print the children based on the indexing scheme
  (call-next-method))

(defmethod print-tree ((node hash-node-children-indexing-mixin) &optional (stream *standard-output*))
  (loop for (key . children) in (list-children node)
       do (progn
	    (format stream "~A -> " key)
	    (format stream "~A" (print-tree children stream))))
  (format stream "]"))

(defmethod list-children ((node hash-node-children-indexing-mixin))
  (let
      ((list '()))
    (maphash (lambda (k v) (push (cons k v) list)) (children node))
    list))


;;; (defmethod print-object ((node node-mixin) stream)
;;;   (print-unreadable-object (node stream :type t :identity t)
;;;     (format stream "parent: ~A" (parent node))))

(defmethod print-object ((node value-node-mixin) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "value: ~A" (value node))))

;; Example:

(defclass my-node (value-node-mixin hash-node-children-indexing-mixin)
  ()
  )



(defparameter *cnode* (make-instance 'my-node :value "copia"))

(defparameter *tree* (make-instance 'my-node :value 32
				    :children `((a . ,(make-instance 'my-node :value 22))
						     (b . ,(make-instance 'my-node :value 13 :children (list (cons :c *cnode*)))))))

(print-tree *tree*)


;; Error: no es posible hacer fat-copy con sentido ya que el puntero al nodo padre hace necesario copiar todo el arbol !!!
;; De todas formas, podemos implementar una búsqueda del nodo padre en lugar de actualizar punteros, aunque esto es costoso en tiempo (orden de ejecución O(n))


(defvar *tree* nil "The tree we are processing")

(defmethod parent ((node node-mixin))
  (node-parent node *tree*))

(defun node-parent (node tree)
  (search-node-parent node tree))

(defun search-node-parent (node tree &optional parent)
  (if (equal node tree)
      parent
      (loop for child in (children tree)
	   do (let ((it (search-node-parent node child tree)))
		(when it
		  (return-from search-node-parent it))))))

(copy-tree-from-node *cnode*)