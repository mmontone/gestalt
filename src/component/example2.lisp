(in-package :gestalt)

(defcomponent toplevel ()
  ((body :component t
	 :serializable t
	 :accessor body
	 :initform (make-instance 'counter :value 0))))

;; In this example we serialize the component "mode". That is, the type of the body component, as it can be
;; a counter or a list

(define-serialization toplevel (component)
    :counter-1 (value (counter-1 component))
    :counter-2 (value (counter-2 component)))

(define-unserialization toplevel (args)
  (let ((toplevel (make-instance 'toplevel)))
    (with-slots (counter-1 counter-2) toplevel
      (setf counter-1 
	    (make-instance 'counter :value (cdr (assoc :counter-1 args))))
      (setf counter-2
	    (make-instance 'counter :value (cdr (assoc :counter-2 args)))))))
