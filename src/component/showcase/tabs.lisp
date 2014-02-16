(in-package :gestalt)

(defcomponent tabs-showcase ()
  ((tabs :component t
	 :initform (make-instance 'tabs-component
				  :tabs `((tab1 . ,(make-instance 'label :value "Tab1!!"))
					  (tab2 . ,(make-instance 'label :value "Tab2!!"))))
	 :accessor tabs))
  (:render (self)
	   (:div (render (tabs self)))))
