(in-package :gestalt)

(defun get-random-string (length &key (alphabetic nil) (numeric nil) (punctuation nil))
  (assert (or alphabetic numeric))
  (let ((alphabet nil))

    (when alphabetic
      (setf alphabet (append alphabet (concatenate 'list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))
    (when numeric
      (setf alphabet (append alphabet (concatenate 'list "0123456789"))))
    (when punctuation
      (setf alphabet (append alphabet (concatenate 'list "!?;:\".,()-"))))

    (setf alphabet (make-array (length alphabet) :element-type 'character :initial-contents alphabet))
    (loop for i from 1 upto length
       collecting (string (elt alphabet (random (length alphabet)))) into pass
       finally (return (apply #'concatenate 'string pass)))))

(defparameter *list1* (loop for i from 1 to 100
			 collect (get-random-string 10 :alphabetic t)))

(defparameter *list2* (loop for i from 1 to 50
			   collect (get-random-string 20 :alphabetic t)))

(defcomponent list-component-1 (list-component)
  ()
  (:default-initargs :items *list1*))

(defcomponent list-component-2 (list-component)
  ()
  (:default-initargs :items *list2*))

(defcomponent lists-showcase ()
  ((list1 :component t
	  :initform (make-instance 'list-component-1)
	  :accessor list1)
   (list2 :component t
	  :initform (make-instance 'list-component-2)
	  :accessor list2))
  (:render (self)
	   (htm (:div
		  (render (list1 self)))
		(:div
		  (render (list2 self))))))
