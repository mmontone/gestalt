(in-package :df.test)

#|
Tests:

(replace-freevars '() '(print "hola") (lambda (x) x))
(replace-freevars '() '(let ((x 2) (y x)) (+ 1 2) (print "hola" x) 'hoa) (lambda (x) 2))
(replace-freevars '() '(let* ((x  (print "hola")) (y (adsf)) (z 34)) (+ 1 2) (print "hola" x) 'hoa) (lambda (x) 2))
(replace-freevars '() '(lambda (x &optional (y 4)) (print x) (print y)) (lambda (x) 2))
(replace-freevars '() '(flet ((my-f () (print x) my-f) (other-f (x) my-f x)) my-f other-f) (lambda (x) 2))
(replace-freevars '() '(labels ((my-f () (print x) my-f) (other-f (x) my-f x)) my-f other-f) (lambda (x) 2))

|#


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