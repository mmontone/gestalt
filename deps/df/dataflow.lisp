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

(defmacro defdbgmacro (name args &rest body)
  `(defmacro ,name ,args
     (when *debug*
	 ,@body)))

(defdbgmacro dbg (&body body)
  `(progn
     ,@body))

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
    `(let* ((,dffcell (make-instance 'dfvaluecell :name (symbol-name ,dffcell)))
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

Nota: en este momento el algoritmo de dataflow se encuentra
en los wlambda creados. Para desacoplar el algoritmo habria que transformar el codigo de forma tal de crear lambdas que bindeen con nuevas variables todas las variables libres. Además, cada celda debería contener una tabla weak a las variables libres. Para ejecutar, habria que pasar esas variables al nuevo lambda generado.

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

(make-let df-vars (lambda (var-name var-value)
		    `(make-instance 'dfvaluecell
				    :name ,(symbol-name var-name)
				    :value ,var-value)))
(make-using df-vars (lambda (binding)
		      `(value ,binding)))

(make-with df-vars)