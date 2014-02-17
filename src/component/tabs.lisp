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

(define-serialization tabs-component
  (append
   (call-next-method)
   
   ;; Serialize the tab labels
   (list (serialize-to-uri (mapcar #'first (tabs component))
		     (path path 'tabs)))

   ;; Serialize the tab components
   (loop for (label . comp) in (tabs component)
      appending
	(serialize-to-uri comp (path path label)))))

(define-unserialization tabs-component
  (let ((tabs
	 (call-next-method)))
    ;; Unserialize the labels
    (let* ((labels (read-from-string (find-argument (path path 'tabs) uri)))
	   (components (loop for label in labels
			    collect
			    (unserialize-component-from-uri uri (path path label)))))
      (setf (tabs tabs)
	    (loop for label in labels
		 for component in components
		 collect (cons label component))))
    tabs)) 

(defmethod initialize-instance :after ((tabs tabs-component) &rest initargs)
  (when (tabs tabs)
    (switch-to-tab tabs (car (first (tabs tabs))))))

(define-renderer ((tabs tabs-component))
  (htm
   (:ul :class "tabs"
	(loop for (label . component) in (tabs tabs)
	   do
	     (htm (:li (:a :href (action-link (action switch-to-tab tabs label))
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
