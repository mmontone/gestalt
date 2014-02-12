(in-package :gestalt)

(defcomponent counter ()
  ((value :initform 0
	  :accessor value
	  :type integer
	  :serialize t
	  :component nil
	  :initarg :value))
  (:render (counter)
	   ;; When rendering a command, the current component path is serialized
	   ;; In this case the current counter path is serialized, and then
	   ;; the callback is invoked with the component in that path
	   (htm (:p (str (princ-to-string (value counter))))
		(input :type "button"
		       :label "Increment"
		       :command #'increment-counter)
		(input :type "button"
		       :label "Decrement"
		       :command #'decrement-counter))))

(defun increment-counter (counter)
  (incf (value counter)))

(defun decrement-counter (counter)
  (decf (value counter)))
