(in-package :df)

(defun weak-pointer-value* (weak-pointer)
  (let ((target (weak-pointer-value weak-pointer)))
    (if target
	(values target t)
	(values nil nil))))

(defmacro wlambda (args &rest body)
  (let*
      ((freevars (make-hash-table :test #'equal))
       (table (make-hash-table :test #'equal))
       (body (macroexpand body)))
    (multiple-value-bind (body non-external-declarations external-vars)
	(prune-externals body)
      (let
	  ((new-body (mapcar (lambda (body-form)
			       (replace-freevars body-form
						 (lambda (freevar)
						   (if (not (member freevar external-vars))
						       (multiple-value-bind (previous-lexvar found)
							   (gethash freevar freevars)
							 (if found
							     previous-lexvar
							     (let ((gen-lexvar (gensym (string freevar))))
							       (setf (gethash freevar freevars) gen-lexvar)
							       gen-lexvar)))
						       freevar))
						 (list-lambda-list-vars args)))
			     body)))
              (let ((freevars-list '()))
		(maphash (lambda (freevar gen-lexvar)
			   (push (cons freevar gen-lexvar) freevars-list))
			 freevars)
		`(let ,(mapcar (lambda (fv)
				 (let* ((freevar (car fv))
					(sym (gensym (string freevar))))
				   (setf (gethash freevar table) sym)
				   (list sym `(make-weak-pointer ,freevar))))
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
						     (weak-pointer-value* ,(gethash freevar table))
						   (setf ,weakref ,target)
						   ,found)))
					    freevars-list))
			 ,@new-body)))))))))

(defmacro dflambda (args &rest body)
  (let*
      ((freevars (make-hash-table :test #'equal))
       (table (make-hash-table :test #'equal))
       (body (macroexpand body)))
    (multiple-value-bind (body non-external-declarations external-vars)
	(prune-externals body)
      (let 
	  ((new-body (mapcar (lambda (body-form)
			  (replace-freevars body-form
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
						  freevar))
					    (list-lambda-list-vars args)))
			body)))
	(let ((freevars-list '()))
	  (maphash (lambda (freevar gen-lexvar)
		     (push (cons freevar gen-lexvar) freevars-list))
		   freevars)
	  `(let ,(mapcar (lambda (fv)                          ;; weakref vars
			   (let* ((freevar (car fv))
				  (sym (gensym (string freevar))))
			     (setf (gethash freevar table) sym)
			     (list sym `(make-weak-pointer ,freevar))))
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
					       (weak-pointer-value ,(gethash freevar table))
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
  ((name :accessor name :initarg :name)
   (value :accessor value :initarg :value)
   (test :accessor test :initarg :test :initform #'eq))
  (:metaclass required-slots-class))

(defmethod print-object ((dfcell dfvaluecell) stream)
  (print-unreadable-object (dfcell stream :type t :identity t)
    (format stream "~A value: ~A test: ~A"
	    (name dfcell)
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
    `(let* ((,dffcell (make-instance 'dfvaluecell :name ,(symbol-name dffcell)))
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

(make-let df-vars (lambda (var-name var-value)
		    `(make-instance 'dfvaluecell
				    :name ,(symbol-name var-name)
				    :value ,var-value)))
(make-using df-vars (lambda (binding)
		      `(value ,binding)))

(make-with df-vars)