(in-package :gestalt)

;; The showcase component

(defcomponent showcase ()
  ((navigation-bar :component t
		   :serializable t)
   (body :component t
	 :serializable t))
  (:render (showcase)
	   
	   )

