(in-package :gestalt)

(defcomponent counters-showcase ()
  ())

(defmethod initialize-instance :after ((counters counters-showcase) &rest initargs)
  (add-component counters 'counter-1 (make-instance 'counter :value 1))
  (add-component counters 'counter-2 (make-instance 'counter :value 2))
  (add-component counters 'counter-3 (make-instance 'counter :value 3)))

(defmethod render ((counters counters-showcase))
  (loop for counter in (child-components counters)
       do (render counter)))

(defcomponent empty-component () ())

(defcomponent embedded-counter ()
  ((body :component t
	 :initform (make-instance 'empty-component)
	 :accessor body)
   (counter :component t
	    :initform (make-instance 'counter :value 0)
	    :accessor counter)))

(define-renderer ((self embedded-counter))
  (render (counter self))
  (if (typep (body self) 'empty-component)
      (htm (:a :href (action-link 'embed-counter)
	       "Embed counter"))
      ;; else
      (render (body self))))

(define-action embed-counter (self &rest args)
  (setf (body self) (make-instance 'embedded-counter)))
