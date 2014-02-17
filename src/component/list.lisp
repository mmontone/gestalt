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
  ((items :accessor items
	  :initform nil	  
	  :initarg :items
	  :documentation "The list")
   (page :accessor page
	 ;; Declaring a default value, we know we have to serialize it only if the slot value
	 ;; is different form the default value (potentially shorter serialization). That's
	 ;; the difference from :initform. Another option is to interecept :initform in the metaclass
	 ;; and consider it the default value (good option!)
	 ;;:default 1
	 :initform 1
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
	      :initform 10
	      :initarg :page-size
	      :documentation "The list page size")
   (navigator-size :accessor navigator-size
		   :initarg :navigator-size
		   :initform 10
		   :serialize t
		   :documentation "The list navigator size")
   (render-item-function :initarg :render-item-function
			 :initform nil
			 :accessor render-item-function
			 :serialize t))  
  (:render (list)
	   (let ((items-page (subseq (items list)
				     (* (page-size list) (page list))
				     (min (1- (* (page-size list) (1+ (page list))))
					  (length (items list))))))
	     (htm (:ul
		    (loop for item in items-page do
			 (htm (:li (render-list-item list item)))))))
	   (multiple-value-bind (pages-number
				 left-page
				 right-page
				 from-result
				 to-result
				 previous-page
				 next-page
				 first-page
				 last-page)
	       (calculate-paginated-list :page (page list)
					 :total-count (length (items list))
					 :window-size (navigator-size list)
					 :page-size (page-size list))
	     (declare (ignore from-result to-result first-page last-page))
	     (htm
	      (:div :id (format nil "~A:nav" (component-path-string list))
		    (:div :class "pages" (str (prin1-to-string pages-number)))
		    (when previous-page
		      (htm
		       (:a :href (action-link (action list-goto-page list left-page))
			   "<")))
		    (loop for c from left-page to right-page
		       do
			 (htm (:a :href (action-link (action list-goto-page list c))
				  (str (prin1-to-string c)))))
		    (when next-page
		      (htm (:a :href (action-link (action list-goto-page list next-page))
			       ">"))))))))

(defmethod render-list-item ((list list-component) item)
  (if (render-item-function list)
      (funcall (symbol-function (render-item-function list))
	       list
	       item)
      ;; else
      (with-html
	(str (prin1-to-string item)))))

(define-action list-goto-page (list page)
  (setf (page list) page))
