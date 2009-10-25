(in-package :df.test)

(def-suite df-tests
    :description "Dataflow tests")

(defun run-tests ()
  (run 'df-tests))

(in-suite df-tests)
	  
; dataflow basic elements

(test value-cell-test
  ;; the value-cell requires a value
  (signals error
    (make-instance 'value-cell))
  ;; notifies its dependents
  (let ((v (make-instance 'value-cell :value 32)))
    (is (equalp (value v) 32))
    (let (set-value)
      (add-dependent v 'changed
		     (lambda (event)
		       (setf set-value (value event))))
      (setf (value v) 22)
      (is (equalp set-value 22))))
  ;; notifies its dependents once only, if the same value is set
  (let ((v (make-instance 'value-cell :value 32))
	(times 0))
    (add-dependent v 'changed
		   (lambda (event)
		     (declare (ignore event))
		     (incf times)))
      (setf (value v) 22)
      (setf (value v) 22)
      (setf (value v) 22)
      (is (equalp times 1))))
  
     

(test formula-test
  (let ((x (make-instance 'value-cell :value 10))
	(y (make-instance 'value-cell :value 10)))
    (let ((f
	   (make-instance 'formula
			  :arguments (list x y)
			  :formula (lambda (x y)
				     (+ x y)))))
      (is (equalp (value f) 20))
      (setf (value x) 20)
      (is (equalp (value f) 30))
      (setf (value y) 20)
      (is (equalp (value f) 40)))))

(test formula-weakness-test
  (let* ((money (make-instance 'value-cell :value 100))
	 (formula
	  (let ((suma (let ((b 2))
			(mk-formula (money)
			  (+ money b)))))
	    (mk-formula (suma)
	      (* suma 5))))
	 (formula-value (value formula)))
    (sb-ext:gc :full t)
    (setf (value money) 12)
    ;; the formula shouldn't have been updated since the binding
    ;; should have been gone with the gc (the binding to the intermediate
    ;; formula in suma should go away)
    (is (value formula) formula-value))

  ; now, if we use mk-lambda, then we should have no problems,
  ; because it binds strongly
  ;; (let* ((money (make-instance 'value-cell :value 100))
  ;; 	 (formula
  ;; 	  (let ((suma (let ((b 2))
  ;; 			(mk-lambda (money)
  ;; 			  (+ money b)))))
  ;; 	    (mk-formula (suma)
  ;; 	      (* suma 5))))
  ;; 	 (formula-value (value formula)))
  ;;   (sb-ext:gc :full t)
  ;;   (setf (value money) 12)
  ;;   (is (not (equalp (value formula) formula-value))))
    )

;; the above test doesnt work because simple closures cannot have dependents (in this case, the formula)

;; design desition 1: global registry of dependents? that way, ANY object can have dependents...
;; design desition 2: add-dependent returns the binding so we can convert it to a weak one afterwards by calling a weakly function on it??
;; (weakly (add-dependent cell 'changed some-object))

; dependency management

(test add-dependent-test
  (let* ((v (make-instance 'value-cell :value 32))
	 (f (make-instance 'formula
			   :arguments (list v)
			   :formula (lambda (v)
				      (+ v v)))))
    ;; adding as dependent twice, throws an error
    (signals dependency-exists
      (add-dependent v 'changed f))))

(test remove-dependent-test
  (let* ((v (make-instance 'value-cell :value 22))
	 (f (make-instance 'formula
			   :arguments (list v)
			   :formula (lambda (v)
				      (+ v v)))))
    (remove-dependent v 'changed f)
    (setf (value v) 10)
    (is (equalp (value f) 44))))
    

; event handling policies

(test events-handling-policy-test)

; events propagation
(test trigger-event-test)

;mop
(test mop-test)

;dataflow syntax
(test mk-formula-test)
(test let-df-vars-test)
(test using-df-vars-test)
(test with-df-vars-test)
(test as-df-vars-test)
(test with-df-slots-test)


;; (defclass bank-account ()
;;   ((money :initform (make-instance 'value-cell :value 0))))

;; (defparameter *ba* (make-instance 'bank-account))


;; (defparameter *s*
;;   (let ((slot (slot-value *ba* 'money)))
;;     (mk-formula (slot)
;;      (let*
;; 	 ((value 44)
;; 	  (suma (+ slot value)))
;;        (format t "Getting value of suma2!!~%")
;;        (* suma value)))))

;; (setf (value (slot-value *ba* 'money)) 2)
;; (setf (value (slot-value *ba* 'money)) 10)

;; ;; Example with the new MOP glue


;; (defclass b-account ()
;;   ((money :initform 0 :dataflow t))
;;   (:metaclass dataflow-class))

;; (defparameter *ba* (make-instance 'b-account))

;; (defparameter *w*
;;   (let ((suma (let ((b 2))
;; 		(with-df-slots (money) *ba*
;; 			       (df
;; 				(declare (external b))
;; 				(+ money b))))))
;;     (df
;;      (format t "Getting value of suma1!!~%")
;;      (* suma 5))))

;; (defparameter *s*
;;   (with-df-slots (money) *ba*
;;     (df
;;      (let*
;; 	 ((value 44)
;; 	  (suma (+ money value)))
;;        (format t "Getting value of suma2!!~%")
;;        (* suma value)))))

;; (setf (slot-value *ba* 'money) 2)
;; (format t "Money:~A~%" (slot-value *ba* 'money))

;; (with-slots (money) *ba*
;;   (setf money 3))