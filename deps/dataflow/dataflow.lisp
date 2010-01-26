(in-package :dataflow)

;----------------------------
; Dataflow cells definitions
;----------------------------

(defclass cell ()
  ((dependents :initform (make-hash-table)
	       :accessor dependents
	       :documentation "The cells dependents table. It is organized by event"))
  (:documentation "A cell is an object that triggers events and has dependents interested in those changes"))

(defclass value-cell (cell)
  ((value :initarg :value
	  :accessor value
	  :initform (error "Provide the value")
	  :documentation "The value of the cell")
   (test :initarg :test
	 :accessor test
	 :initform #'eql))
  (:documentation "A cell that contains a changing value")
  (:metaclass required-slots-class))

(defclass standard-event (standard-class)
  ()
  (:documentation "Just to do some event matching in method parameters"))

(defmethod validate-superclass ((class standard-event)
				(super standard-class))
  t)

(defmethod hash ((event-class standard-event))
  event-class)

(defmethod event-dependents ((event symbol) (cell cell))
  (event-dependents (find-class event) cell))

(defmethod event-dependents ((event standard-event) (cell cell))
  (gethash event (dependents cell)))


(defclass event ()
  ((triggerer :initarg :triggerer
	      :accessor triggerer
	      :initform (error "Provide the triggerer")
	      :documentation "The event triggerer"))
  (:metaclass standard-event)
  (:documentation "The events superclass"))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream)
    (format stream "triggerer: ~A" (triggerer event))))

(defclass changed (event)
  ((value :initarg :value
	  :accessor value
	  :initform (error "Provide the value")
	  :documentation "The new triggerer value"))
  (:metaclass standard-event)
  (:documentation "Signaled when an object changes"))

(defmethod print-object ((event changed) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "~A changed to: ~A"
	    (triggerer event)
	    (value event))))

(defclass formula (cell)
  ((result :documentation "The formula result")
   (arguments :initarg :arguments
	      :accessor arguments
	      :initform (error "Provide the arguments")
	      :documentation "The arguments of the formula. They are weakly bound")
   (formula :initarg :formula
	    :accessor formula
	    :initform (error "Provide the formula")
	    :documentation "The formula. A function that takes the objects pointed by the arguments slot"))
;  (:metaclass funcallable-standard-class))
  (:documentation "A formula cell is a cell that calculates a value based on arguments cells"))

(defmethod value ((cell formula))
  (result cell))

(defmethod result ((formula formula))
  (when (not (slot-boundp formula 'result))
    (setf (slot-value formula 'result) (evaluate-formula formula)))
  (slot-value formula 'result))

(defmethod print-object ((formula formula) stream)
  (print-unreadable-object (formula stream :type t :identity t)
    (format stream "value: ~A" (result formula))))

(defmethod initialize-instance :after ((cell formula) &rest initargs)
  (declare (ignore initargs))
  ;; (set-funcallable-instance-function cell
  ;; 				     (lambda (event)
  ;; 				       (declare (ignore event))
  ;; 				       (evaluate-formula cell)))
  ;; Keep weak-references to the arguments only
  (let ((args (copy-list (arguments cell))))
    (setf (arguments cell)
	  (loop for arg in args
	     collect (make-weak-pointer arg)))
    (loop for arg in args
       do (add-dependent arg 'changed cell)))
  (evaluate-formula cell))

(defun evaluate-formula (cell)
  (let ((args (loop for arg in (arguments cell)
		 for c = (weak-pointer-value arg)
		 when (null c)
		 do (return-from evaluate-formula
		      (values nil nil))
		 collect (value c))))
    (let ((result (apply (formula cell) args)))
      (setf (slot-value cell 'result) result)
      (log-for df "~A evaluated" cell)
      (trigger-event 'changed :triggerer cell :value result)
      (values result t))))

;-----------------------------
; Cells dependents management
;-----------------------------

(define-condition dependency-exists (serious-condition)
  ((cell :initarg :cell
	 :reader cell
	 :initform (error "Provide the cell"))
   (event :initarg :event
	  :reader event
	  :initform (error "Provide the event"))
   (dependent :initarg :dependent
	      :reader dependent
	      :initform (error "Provide the dependent")))
  (:report (lambda (c s)
	     (format s "~A is already registered as dependent of ~A for event ~A"
		     (dependent c)
		     (cell c)
		     (event c))))
  (:documentation "The dependent we are trying to add already exists"))

(defun dependency-binding-target (binding)
  (if (weak-pointer-p binding)
      (weak-pointer-value binding)
      binding))

(defgeneric add-dependency-binding (binding cell event &key if-exists)
  (:documentation "Adds a dependency binding to a cell. A dependency binding can be either a weak or a direct (strong) reference to the dependent object")
  (:method (binding (cell cell) (event symbol) &key (if-exists :error))
    (add-dependency-binding binding cell
			    (aif (find-class event)
			     it
			     (error "The event is not valid"))
			    :if-exists if-exists))
  (:method (binding (cell cell) (event standard-event)
	    &key (if-exists :error))
    (labels ((%add-dependency-binding ()
	       (symbol-macrolet ((events-dependents
				  (gethash (hash event) (dependents cell))))
		 (flet ((push-binding (binding)
			  ;; Hook for finalization
			  (when (weak-pointer-p binding)
			    (trivial-garbage:finalize
			     (weak-pointer-value binding)
			     (lambda ()
			       (log-for df "Removed obsolete ~A from ~A list of dependents" binding cell)
			       (setf events-dependents
				     (delete binding events-dependents)))))
			  ;; Add the binding
			  (push binding events-dependents)
			  binding))
		 (multiple-value-bind (list found-p) events-dependents
		   (declare (ignore list))
		   (when (not found-p)
		     (setf events-dependents '())))
		 (restart-case
		     (progn
		       (loop
			  with deps = events-dependents
			  for dep in deps
			  when
			    (eql (dependency-binding-target dep)
				   (dependency-binding-target binding))
			  do (error 'dependency-exists
				    :cell cell
				    :event event
				    :dependent (dependency-binding-target dep)))
		       (push-binding binding))
		   (superceed ()
		     (push-binding binding)))))))
      (ecase if-exists
	(:error (%add-dependency-binding))
	(:superseed (handler-bind 
			((dependency-exists (lambda (c)
					      (declare (ignore c))
					      (invoke-restart 'superceed))))
		      (%add-dependency-binding)))))))

(defgeneric add-dependent (cell event dependent &key if-exists)
  (:documentation "Adds a dependent to a cell")
  (:method (cell event dependent &key (if-exists :error))
    ;; add dependents weakly in general
    (log-for df "Adding dependent ~A to ~A for event ~A"
	     dependent cell event)
    (add-dependency-binding (make-weak-pointer dependent)
			    cell event
			    :if-exists if-exists))
  (:method (cell event (dependent function) &key (if-exists :error))
    ;; in the case of a function, add dependent strongly
    (log-for df "Adding dependent ~A to ~A for event ~A"
	     dependent cell event)
    (add-dependency-binding dependent cell event
			    :if-exists if-exists)))

(defgeneric remove-dependent (cell event dependent)
  (:documentation "Removes a dependent from the cell")
  (:method ((cell cell) (event symbol) dependent)
    (if (equalp event :all)
	(maphash (lambda (ev tbl)
		   (declare (ignore tbl))
		   (remove-dependent cell ev dependent))
		 (dependents cell))
	(remove-dependent cell (find-class event) dependent)))
  (:method ((cell cell) (events cons) dependent)
    (loop for event in events
       do (remove-dependent cell event dependent)))
  (:method ((cell cell) (event standard-event) dependent)
    (let ((event-dependents (gethash (hash event) (dependents cell))))
      (loop for binding in event-dependents
	 when (eql (dependency-binding-target binding)
		   dependent)
	 do (setf (gethash (hash event) (dependents cell))
		    (delete binding event-dependents))))))

(defgeneric dependents-of-event (event cell)
  (:documentation "Returns all the dependents for a cell on some event")
  (:method ((event standard-event) (cell cell))
    (flatten
     (mapcar #'dependency-binding-target
	     (gethash (hash event) (dependents cell))))))

(defmacro print-cell ((cell stream) &body body)
  (once-only (cell)
    `(print-unreadable-object (,cell ,stream :type t :identity t)
       ,@body)))

(defmethod print-object ((cell cell) stream)
  (print-cell (cell stream)))

(defmethod print-object ((cell value-cell) stream)
  (print-cell (cell stream)
    (format stream "value: ~A test: ~A"
	    (if (slot-boundp cell 'value)
		(value cell)
		"#unbound")
	    (test cell))))
 
(defgeneric evaluate-dependent (dependent event)
  (:documentation "(Re)evaluates a dependent because an event happened")
  (:method ((dependent formula) event)
    (log-for df "Evaluating dependent: ~A event: ~A" dependent event)
    (evaluate-formula dependent))
  (:method ((dependent function) event)
    (log-for df "Evaluating dependent: ~A event: ~A" dependent event)
    (funcall dependent event)))

;--------------------------
; Events handling policy
;--------------------------

(defvar *events-handling-policies*
  (make-hash-table :test #'equalp)
  "A table holding the events handling policies")

(defvar *events-handling-policy* :default
  "The event-handing policy for when events are propagated")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-event-handling-policy (name args &body body)
    (with-unique-names (event-handler-policy-function)
      `(flet ((,event-handler-policy-function ,args
		,@body))
	 (setf (gethash ',name  *events-handling-policies*)
	       #',event-handler-policy-function))))

  (defmacro with-events-handling-policy (policy &body body)
    `(let
	 ((*events-handling-policy* ,policy))
       ,@body)))

(define-event-handling-policy :default (event events)
  (declare (ignore events))
  (propagate-event event))

(define-event-handling-policy :defer-events (event events)
  (error "fix this. pushing to the events variable doesn't work")
  (push event events))

(define-event-handling-policy :once-only (event events)
  (declare (ignore event events))
  (error "TODO"))

(defun get-handling-policy-function (policy)
  (multiple-value-bind (function found-p)
      (gethash policy
	       *events-handling-policies*)
    (when (not found-p)
      (error "The events handling policy ~A was not found" policy))
    function))

;----------------------
; Events propagation
;----------------------

(defvar *propagating* nil "t when we are propagating changes")

(defvar *restoring-functions* '()
  "Command pattern functions for restoring cells values
after an error has ocurred")

(define-condition trigger-event-signal ()
    ((event :initarg :event
	    :accessor event
	    :initform (error "Provide the event")
	    :documentation "The event that ocurred"))
    (:documentation "This condition is meant to be caught by the propagation algorithm when an event ocurrs"))

(defgeneric trigger-event (event &rest args)
  (:documentation "Triggers an event")
  (:method ((event event) &rest args)
    (declare (ignore args))
    (if *propagating*
        (signal 'trigger-event-signal :event event)
	;else
	(propagate-event event :handling-policy *events-handling-policy*)))
  (:method ((event symbol) &rest args)
    (trigger-event (apply #'make-instance event args))))

(defmethod trigger-event :around ((event event) &rest args)
  (if *propagating*
      (apply #'call-next-method event args)
      (restart-case
	  (apply #'call-next-method event args)
	(restore-values ()
	  :report (lambda (s)
		    (format s "Restore the cells values"))
	  (loop for command in *restoring-functions*
	       do (funcall command))))))

(defun propagate-event (event &key (handling-policy :default))
  (let ((events (list event))
	(policy-function (get-handling-policy-function handling-policy)))
    (handler-bind
	((trigger-event-signal
	  (lambda (signal)
	    (funcall policy-function (event signal) events)
	    (continue))))
      (let ((*propagating* t))
	(loop
	   for event in events
	     do (loop
		for dependent in (dependents-of-event (class-of event) (triggerer event))
		do (evaluate-dependent dependent event)))))))

(defmethod (setf value) (new-value (cell value-cell))
  (flet ((set-value ()
	   (when (or (not (slot-boundp cell 'value))
		     (not (funcall (test cell) new-value (value cell))))
	     (let ((old-value (slot-value cell 'value)))
	       (log-for df "Setting value of: ~A  to: ~A~%" cell new-value)
	       (setf (slot-value cell 'value) new-value)
	       (push (lambda () (setf (slot-value cell 'value) old-value))
		     *restoring-functions*)
	       (trigger-event 'changed
			      :triggerer cell
			      :value new-value))
	     new-value)))
    (if *propagating*
	(set-value)
	(let ((*restoring-functions* '()))
	  (set-value)))))

;-------------------------
;  Dataflow syntax
;-------------------------

(defmacro mk-formula (args &body body)
  `(make-instance 'formula
		  :arguments (list ,@args)
		  :formula (lambda ,args
			     ,@body)))

(defmacro mk-lambda (args &body body)
  (with-unique-names (func event)
    `(flet ((,func (,event) (declare (ignore ,event)) ,@body))
       ,@(loop for arg in args
	    collect `(add-dependent ,arg 'changed #',func))
       #',func)))


(make-let df-vars (lambda (var-name var-value)
		    `(make-instance 'value-cell
				    :name ,(symbol-name var-name)
				    :value ,var-value)))
(make-using df-vars (lambda (binding)
		      `(value ,binding)))

(make-with df-vars)

(make-as df-vars)