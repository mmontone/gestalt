(in-package :gestalt)

(defcomponent tabs-component ()
  ((tabs :initarg :tabs
	 :accessor tabs
	 :initform nil)
   (active-tab :initarg :active-tab
	       :accessor active-tab
	       :initform nil
	       :serialize t)))

(defmethod render ((tabs tabs-component))
  (with-html-output (*http-stream*)
    (htm
     (:ul :class "tabs"
	   (loop for (label . component) in tabs
	      do
		(htm (:li (str label)))))
     (:div :class "body"
	   (render (active-tab tabs))))))
