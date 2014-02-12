(in-package :gestalt)

(defun calculate-paginated-list (&key page total-count (window-size 10)
				 (page-size 10))
  (let* ((pages-number (multiple-value-bind (number remainder)
			     (truncate (/ total-count page-size))
			   (+ number
			    (if (plusp remainder) 1 0))))
	 (wnd-diff (truncate (/ window-size 2)))
	 (right-page (min pages-number
			  (max (+ page wnd-diff)
			       window-size)))
	 (left-page (max 1
			 (min (- page wnd-diff)
			      (- page (- window-size (1+ (- pages-number page)))))))
	 (from-result (1+ (* (- page 1) page-size)))
	 (to-result (min (- (+ from-result page-size) 1)
			 total-count))
	 (next-page
	  (if (< page pages-number)
	      (1+ page)))
	 (previous-page
	  (if (> page 1)
	      (- page 1)))
	 (first-page
	  (if (> page 1)
	      1))
	 (last-page
	  (if (< page pages-number)
	      pages-number)))
    (assert (>= page 1) nil
	"Page cannot be lower than 1")
    (assert (or (zerop total-count)
		(<= page pages-number)) nil
	    "Page cannot be greater than number of pages: ~A" pages-number)
    (assert (>= window-size 3) nil
	"window-size cannot be lower than 3")
    (values
     pages-number
     left-page
     right-page
     from-result
     to-result
     previous-page
     next-page
     first-page
     last-page)))

(defcomponent list-component ()
  ((value :accessor value
	  :initform nil	  
	  :initarg :value
	  :documentation "The list")
   (page :accessor page
	 ;; Declaring a default value, we know we have to serialize it only if the slot value
	 ;; is different form the default value (potentially shorter serialization). That's
	 ;; the difference from :initform. Another option is to interecept :initform in the metaclass
	 ;; and consider it the default value (good option!)
	 :default 1
	 :type integer
	 :serialize t
	 :component nil
	 :initarg :page
	 :documentation "The current page")
   (page-size :accessor page-size
	      :serialize t
	      ;; Declaring a default value, we know we have to serialize it only if the slot value
	      ;; is different form the default value (potentially shorter serialization)
	      ;:default 10
	      :initarg :page-size
	      :documentation "The list page size")
   (navigator-size :accessor navigator-size
		   :initarg :navigator-size
		   :initform 10
		   :serialize t
		   :documentation "The list navigator size"))  
  (:render (list)
	   (htm (:ul
		  (loop for item in (value list) do
		       (html (:li
			       (funcall (item-renderer list) item)))))
		(:div :id (component-path-string component)
		      (


	      
	      
   
