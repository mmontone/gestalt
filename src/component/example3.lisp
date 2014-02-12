(in-package :gestalt)

(defcomponent toplevel ()
  ((counter-1 :component t
	      :accessor counter-1
	      :initform (make-instance 'counter :value 0))
   (counter-2 :component t
	      :accessor counter-2
	      :initform (make-instance 'counter :value 0)))
  (:render (component)
	   (with-html-output (*http-stream*)
	     (htm (:div
		   (:h1 (str "Counter 1"))
		   (render (counter-1 component)))
		  (:div
		   (:h1 (str "Counter 2"))
		   (render (counter-2 component)))))))

;; In this example we only need to serialize the counter values; it is not necessary
;; to serialize the type of the toplevel child components (counter) because they are fixed
;; (they are always of type counter)

#+nil(define-serialization toplevel
    (list (cons (path path 'counter-1) (value (counter-1 component)))
	  (cons (path path 'counter-2) (value (counter-2 component)))))

#+nil(define-unserialization toplevel
  (let ((toplevel (make-instance 'toplevel)))
    (with-slots (counter-1 counter-2) toplevel
      (setf counter-1 
	    (make-instance 'counter :value (find-argument (path path 'counter-1) uri)))
      (setf counter-2
	    (make-instance 'counter :value (find-argument (path path 'counter-2) uri))))))
