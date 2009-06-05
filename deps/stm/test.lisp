;; These are the stm tests

(let-stm-vars ((x 33))
	 (print-hola)
	 )

(using-stm-vars (x y)
  (print x))

(with-stm-vars ((x 34) (y 44))
  (print x))

(defunstm stmtest (y)
  (print y))

 (stm
 (print x)
 (let ((x y))
   (print x))
 (setf x 34)
 (stmtest w)
 )


Some examples we need to make work:

with-stm-vars should check the types of its arguments.
They should be either stm-vars or refs. If they are refs, then it creates stm-vars from them (wraps them). Then it binds the stm-vars through symbol-macrolet. If arguments are not stm-vars or refs, we throw a typing error.

(let-refs
    ((x 23)
     (y 45))
  (with-stm-vars (x y)
    (stm
     (atomically
      (setf x 23)
      (setf y 44))))
  (list x y))

(let
    ((x 23)
     (y 45))
  (with-stm-vars (x y)
    (stm
     (atomically
      (setf x 23)
      (setf y 44))))
  (list x y)) ;; We won't see the commited values

		 
(let
    ((x 23)
     (y 45))
  (with-stm-vars (x y)
    (stm
     (atomically
      (setf x 23)
      (setf y 44)))
    (list x y))) ;; We see the commited values

;; Rollbacks

(let-refs
    ((x 23)
     (y 45))
  (with-stm-vars (x y)
    (stm
     (begin-transaction)
     (format t "Edition started~%")
     (setf x 23)
     (setf y 44)
     (format t "x and y changed to ~A and ~A~%" x y)
     (roll-back)
     (format t "Edition cancelled~%")
     (format "x and y values are ~A and ~A~%" x y)
     ))
  (list x y))

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
