(in-package :gestalt)

(defcomponent tabs-component ()
  ((tabs :initarg :tabs
	 :accessor tabs
	 :initform nil)
   (active-tab :initarg :active-tab
	       :accessor active-tab
	       :serialize t)
   (active-component :accessor active-component
		     :component t)))

(defmethod initialize-instance :after ((tabs tabs-component) &rest initargs)
  (switch-to-tab tabs (car (first (tabs tabs)))))

(define-renderer ((tabs tabs-component))
  (htm
   (:ul :class "tabs"
	(loop for (label . component) in (tabs tabs)
	   do
	     (htm (:li (:a :href (action-link switch-to-tab tabs label)
			   (str label))))))
   (:div :class "body"
	 (render (active-component tabs)))))

(define-action switch-to-tab (tabs label)
  (let ((tab (assoc label (tabs tabs))))
    (when (not tab)
      (error "Tab labelled ~A not found in ~A" label tabs))
    (setf (active-tab tabs) label)
    (setf (active-component tabs)
	  (cdr (assoc label (tabs tabs))))))
