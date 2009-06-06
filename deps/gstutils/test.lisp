(in-package :gstutils.test)

(def-suite gstutils-suite
    :description "The Gestalt utils test suite")

(in-suite gstutils-suite)

(test dynamic-variable-symbol-p
  (is (not (gstutils::dynamic-variable-symbol-p 'hola)))
  (is (gstutils::dynamic-variable-symbol-p '*hola*))
  (is (not (gstutils::dynamic-variable-symbol-p '*hola)))
  (is (not (gstutils::dynamic-variable-symbol-p 'hola*))))

(test list-free-vars
  (is (not (set-exclusive-or (list-free-vars '((print x) (print y))) '(x y))))
  (is (not (set-exclusive-or (list-free-vars '((print x) (print y) (let ((z 3)) z))) '(x y))))
  (is (not (set-exclusive-or (list-free-vars '((print x) (print y) z)) '(x y z))))

  ;; in presence of a lexical environment
  (is (not (set-exclusive-or (list-free-vars '((print x) (print y)) '(x)) '(y)))))

(test replace-free-vars-test

  (is (equalp
       (replace-freevars '(print "hola")
			 (lambda (x) x))
       '(print "hola")))

  (is (equalp
       (replace-freevars  '(let ((x 2) (y x))
			    (+ 1 2)
			    (print "hola" x)
			    'hoa)
			 (lambda (x) 2))
       '(LET ((X 2) (Y 2))
	 (+ 1 2)
	 (PRINT "hola" X)
	 'HOA)))

  (is (equalp
       (replace-freevars  '(let* ((x  (print "hola"))
				 (y x)
				 (z 34))
			   (+ 1 2)
			   (print "hola" x)
			   'hoa)
			 (lambda (x) 2))
       '(LET* ((X (PRINT "hola")) (Y X) (Z 34))
	 (+ 1 2)
	 (PRINT "hola" X)
	 'HOA)))

  (is (equalp
       (replace-freevars '(lambda (x &optional (y 4))
			   (print x)
			   (print y))
			 (lambda (x) 2))
       '(LAMBDA (X &OPTIONAL (Y 4))
	 (PRINT X) (PRINT Y))))
  
  (is (equalp
       (replace-freevars '(flet ((my-f () (print x) my-f)
				 (other-f (x) my-f x))
			   my-f other-f)
			 (lambda (x) 2))
       '(FLET ((MY-F ()
		(PRINT 2)
		2)
	       (OTHER-F (X)
		2
		X))
	 MY-F
	 OTHER-F)))

  (is (equalp
       (replace-freevars '(labels ((my-f () (print x) my-f)
				   (other-f (x) my-f x))
			   my-f other-f)
			 (lambda (x) 2))
       '(LABELS ((MY-F ()
		  (PRINT 2)
		  MY-F)
		 (OTHER-F (X)
		  MY-F
		  X))
	 MY-F
	 OTHER-F)))
       
  )

;; required-slots-class tests

(defclass person ()
  ((name :initarg :name :required t)
   (lastname :initarg :lastname :required t :error-msg "Please give me a lastname!!")
   (phone :initarg :phone :initform "" :required nil)
   (address :initarg :address :initform ""))
  (:metaclass required-slots-class)
  (:documentation "The class definition to test required slots"))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type t :identity t)
    (format stream "name: ~A lastname: ~A phone: ~A address: ~A"
	    (slot-value person 'name)
	    (slot-value person 'lastname)
	    (slot-value person 'phone)
	    (slot-value person 'address))))

(test required-slots-class-test
  (signals required-slot-error (make-instance 'person))
  (signals required-slot-error (make-instance 'person :name "Mariano"))
  (signals required-slot-error (make-instance 'person :lastname "Montone"))
  (finishes (make-instance 'person :name "Mariano" :lastname "Montone"))
  (signals required-slot-error (make-instance 'person :address "Mi casa")))