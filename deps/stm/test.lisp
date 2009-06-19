;; These are the stm tests

(in-package :stm.test)

(def-suite stm-suite
    :description "Software Transactional Memory test suite")

(in-suite stm-suite)

(test no-transaction-error-test
  ;; We modify stm vars outside a transaction. That should raise a no-transaction-error
  (signals no-transaction-error
    (with-refs
	((x 32)
	 (y 27))
      (with-stm-vars ((x x)
		      (y y))
	(setf x 34)))))

(test successful-commit-test
  (let ((result
	 (let-refs
	  ((x 23)
	   (y 45))
	  (with-stm-vars ((x x) (y y))
	    (atomically
	      (setf x 34)
	      (setf y 44)))
	  (list x y))))
    (is (= (value (car result)) 34))
    (is (= (value (cadr result)) 44))))

(test optimistic-semantics-test
  (let-refs
   ((x 23)
    (y 45))
   (with-stm-vars ((x x)
		   (y y))
     (atomically
       (setf x 34)
       (setf y 44)
       (is (= x 34))
       (is (= y 44))))))

(test rollback-test
  (let ((result
	 (let-refs
	  ((x 23)
	   (y 45))
	  (with-bindings-as-stm-vars (x y)
	    (begin-stm-transaction)
	    (format t "Edition started~%")
	    (setf x 20)
	    (setf y 44)
	    (format t "x and y changed to ~A and ~A~%" x y)
	    (rollback-stm-transaction))
	  (using-refs (x y)
	    (format t "Edition cancelled~%")
	    (format t "x and y values are ~A and ~A~%" x y)
	    (list x y)))))
    (is (equalp result '(23 45)))))

;; Restarts
(define-condition some-condition (serious-condition)
  ())

(defun some-function ()
  (with-simple-restart (continue "Continue the transaction")
    (error 'some-condition)))

(defun transactional-function (x y &optional tries)
  (with-stm-vars
      ((x x)
       (y y))
    (atomically
      (when tries
	(using-refs (tries)
	  (incf tries)))
      (setf x 33)
      (setf y 23)
      (some-function)))
  (using-refs (x y)
    (list x y)))

(test retry-transaction-test
  (let-refs
   ((x 22)
    (y 27)
    (tries 0))
   (block try
     (handler-bind 
       ((some-condition (lambda (c)
			  (declare (ignore c))
			  (using-refs (tries)
			    (if (= tries 2)
				(return-from try)
				(invoke-restart 'retry-transaction))))))
       (transactional-function x y tries)))
   ;; Check that we made 2 retries and the transaction was rolled-back
   (using-refs (x y tries)
     (is (= x 22))
     (is (= y 27))
     (is (= tries 2)))))

(let-refs
   ((x 22)
    (y 27)
    (tries 0))
   (block try
     (handler-bind 
       ((some-condition (lambda (c)
			  (declare (ignore c))
			  (using-refs (tries)
			    (if (= tries 2)
				(return-from try)
				(invoke-restart 'retry-transaction))))))
       (transactional-function x y tries)))
   ;; Check that we made 2 retries and the transaction was rolled-back
   (using-refs (x y tries)
     (list x y tries)))

(test continue-transaction-test
  (let-refs
     ((x 22)
      (y 27))
     (let ((result
	    (handler-bind ((some-condition(lambda (c)
					    (declare (ignore c))
					    (continue))))
	      (transactional-function x y))))
       (is (equalp result '(33 23))))))

(test abort-transaction-test
  (let-refs
     ((x 22)
      (y 27))
     (let ((result
	    (handler-bind 
	      ((some-condition (lambda (c)
				 (declare (ignore c))
				 (invoke-restart 'abort-transaction))))
	      (transactional-function x y))))
       (is (equalp result '(22 27))))))


#|
(with-stm-vars ((x x-ref) (y y-ref))
  (print x))

(defunstm stmtest (y)
  (print y))

 (stm
 (print x)
 (let ((x y))
   (print x))
 (setf x 34)
 (stmtest w)
 (hola x))


Some examples we need to make work:

with-stm-vars should check the types of its arguments.
They should be either stm-vars or refs. If they are refs, then it creates stm-vars from them (wraps them). Then it binds the stm-vars through symbol-macrolet. If arguments are not stm-vars or refs, we throw a typing error.




;; Dataflow integration

(let-df
 ((x 23)
  (y 45))
 (define-simple-observer x-simple-state x (value old-value)
     (format t "[UI] ~A changed from ~A to ~A" target old-value value))
 (define-simple-observer y-simple-state y (value old-value)
			 (format t "[UI] ~A changed from ~A to ~A" target old-value value))
 (edit x y))

;; El modelo está mal. Tenemos que hacer los cambios efectivos cada vez que se cambia una variable, aunque se esté dentro de una transacción. Después se hace rollback si hay error. Eso es la semántica optimista. Sino nos perdemos de consitencia en el modelo, etc.

(defun my-read (&optional (type t)
		(stream *standard-input*)
		(on-error (lambda (value) (declare (ignore value)))))
  (let
      ((value (read stream)))
    (when (not (typep value type))
      (return-from my-read (funcall on-error value on-error)))
    value))

(defun edit (x y)
  (with-stm-vars (x y)
   (begin-transaction)
  (loop while t
       do (progn
	    (format t "Enter a number for x:")
	    (setf x
		  (my-read 'integer *standard-input* (lambda (value self)
						 (declare (ignore value))
						 (format t "That is not a number.~%")
						 (my-read 'integer *standard-input* self))))
	    (format t "Enter a number for y:")
	    (setf y
		  (my-read 'integer *standard-input* (lambda (value self)
						 (declare (ignore value))
						 (format t "That is not a number.~%")
						 (my-read 'integer *standard-input* self))))
	    (format t "Accept changes/Cancel??(a/c):")
	    (let ((what-to-do)
		   (my-read 'symbol))
	      (loop while (not (or (equal what-to-do 'a)
				   (equal what-to-do 'b)))
		   (format t "I don't understand. ~%")
		   (setf what-to-do (my-read 'symbol)))
	      (if (equal what-to-do 'a)
		  (commit)
		  (rollback)))))))


;; A conflict and versioning test:

(defun proc1 (stm-var)
  (flet (proc2 ()
	       (proc2 stm-var))
    (using-stm-var (stm-var)
		   (atomically
		    (setf stm-var 22)
		    (proc2)
		    (setf stm-var 40)))))

(defun proc2 (stm-var)
  (using-stm-var (stm-var)
	(atomically
	 (setf stm-var 33))))

|#