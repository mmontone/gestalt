(in-package :gestalt)

(defparameter *demos*
  (flet ((source (filename)
	   (asdf:system-relative-pathname :gestalt (format nil "src/component/showcase/~A" filename))))
    `(("Counters"
       :component counters-showcase
       :source  ,(source "counters"))
      ("Lists"
       :component lists-showcase
       :source ,(source "lists"))
      ("Tabs"
       :component tabs-showcase
       :source ,(source "tabs"))
      ("Embedded"
       :component showcase
       :source ,(source "showcase")))))      

;; The showcase component

(defcomponent showcase ()
  ((selected-demo :serialize t
		  :initform 'counters
		  :accessor selected-demo)
   (body :component t
	 :serialize t
	 :accessor body))
  (:initialize (showcase)
	       (setf (body 
  (:render (showcase)
	   
	   (render (body showcase))
	   
	   
	   

