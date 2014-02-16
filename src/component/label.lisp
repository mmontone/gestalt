(in-package :gestalt)

(defcomponent label ()
  ((value :initarg :value
	  :initform nil
	  :serialize t
	  :accessor value))
  (:render (label)
	   (htm (:span (str (value label))))))
