(in-package :gestalt)

(defcomponent counter ()
  ((value :initform 0
	  :accessor value
	  :type integer
	  :serialize t
	  :initarg :value))
  (:render (counter)
	   (htm (:p (str (princ-to-string (value counter))))
		(:a :href (action-link 'increment-counter)
		    "Increment")
		(:a :href (action-link 'decrement-counter)
		    "Decrement"))))

(define-action increment-counter (counter)
  (incf (value counter)))

(define-action decrement-counter (counter)
  (decf (value counter)))
