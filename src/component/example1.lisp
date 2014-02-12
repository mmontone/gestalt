(in-package :gestalt)

(defcomponent toplevel ()
  ((counter-1 :component t
	      :serializable t
	      :accessor counter-1
	      :initform (make-instance 'counter :value 0))
   (counter-2 :component t
	      :serializable t
	      :accessor counter-2
	      :initform (make-instance 'counter :value 0)))
  (:render (component)
	   (with-html-output (*http-stream*)
	     (htm (:div
		   (:h1 (str "Counter 1"))
		   (esc (render (counter-1 component))))
		  (:div
		   (:h1 (str "Counter 2"))
		   (esc (render (counter-2 component))))))))

;; In this example we only need to serialize the counter values; it is not necessary
;; to serialize the type of the toplevel child components (counter) because they are fixed
;; (they are always of type counter)

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
