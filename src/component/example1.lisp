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
