;; lwt example behind Hunchentoot

(defpackage :lwt
  (:use :cl :hunchentoot :parenscript :cl-who))

(in-package :lwt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    `(with-html-output-to-string (*standard-output* nil :prologue t)
       ,@body))
  (defmacro add-dispatchers (&rest dispatchers)
    `(setf lwt::*dispatch-table*
	   (nconc (list ,@dispatchers)
		  lwt::*dispatch-table*))))

(defmethod convert-tag-to-string-list ((tag (eql :js-script)) attr-list body body-fn)
  (funcall body-fn
	   `((:script :type "text/javascript"
		      (str (format nil "~%// <![CDATA[~%"))
		      ;; If we use esc here then code is escaped and that makes it invalid
		      (str (ps ,@body))
		      (str (format nil "~%// ]]>~%"))))))

(defun tutorial1 ()
  (with-html
    (:html
     (:head (:title "ParenScript tutorial: 1st example"))
     (:body (:h1 "ParenScript tutorial: 1st example")
	    (:p "Please click the link below." :br
		((:a :href "#" :onclick (ps-inline
					 (alert "Hello World")))
		 "Hello World"))))))

(defun tutorial1-file ()
  (ps (defun greeting-callback ()
	(alert "Hello World"))))

(defun tutorial1 ()
  (with-html
    (:html
     (:head
      (:title "ParenScript tutorial: 3rd example")
      ((:script :language "JavaScript" :src "/tutorial1.js")))
     (:body
      (:h1 "ParenScript tutorial: 3rd example")
      (:p "Please click the link below." :br
	  ((:a :href "#" :onclick (ps-inline (greeting-callback)))
	   "Hello World")
	  :br "And maybe this link too." :br
	  ((:a :href "#" :onclick (ps-inline (greeting-callback)))
	   "Knock knock")
	  :br "And finally a third link." :br
	  ((:a :href "#" :onclick (ps-inline (greeting-callback)))
	   "Hello there"))))))

(defun slideshow ()
  (with-html
    (:html
     (:head (:title "ParenScript slideshow")
	    ((:script :language "JavaScript"
		      :src "/slideshow.js"))
	    (:js-script
	     (defvar *linkornot* 0)
	     (defvar photos (array "/photo1.jpg"
				   "/photo2.jpg"
				   "/photo3.jpg"))))
     (:body
      (:h1 "ParenScript slideshow"))
     ((:table :border 0
	      :cellspacing 0
	      :cellpadding 0)
      (:tr ((:td :width "100%" :colspan 2 :height 22)
	    (:center
	     (:js-script
	      (let ((img (ps-html
			  ((:img :src (aref photos 0)
				 :name "photoslider"
				 :style (+ "filter:"
					   (lisp (ps (reveal-trans
						      (setf duration 2)
						      (setf transition 23)))))
				 :border 0)))))
		(document.write
		 (if (= *linkornot* 1)
		     (ps-html ((:a :href "#"
				   :onclick (lisp (ps-inline (transport))))
			       img))
		     img)))))))
      (:tr ((:td :width "50%" :height "21")
	    ((:p :align "left")
	     ((:a :href "#"
		  :onclick (ps-inline
			    (progn (backward)
				   (return false))))
	      "Previous Slide")))
	   ((:td :width "50%" :height "21")
	    ((:p :align "right")
	     ((:a :href "#"
		  :onclick (ps-inline
			    (progn (forward)
				   (return false))))
	      "Next Slide"))))))))

;;; The actual slideshow application is generated by the function
;;; `JS-SLIDESHOW', which generates a ParenScript file. Symbols are
;;; converted to JavaScript variables, but the dot "." is left as
;;; is. This enables convenient access to object slots without using
;;; the `SLOT-VALUE' function all the time. However, when the object
;;; we are referring to is not a variable, but for example an element
;;; of an array, we have to revert to `SLOT-VALUE'.

(defun js-slideshow ()
  (ps
    (defvar *preloaded-images* (make-array))
    (defun preload-images (photos)
      (dotimes (i photos.length)
	(setf (aref *preloaded-images* i) (new *Image)
	      (slot-value (aref *preloaded-images* i) 'src)
	      (aref photos i))))

    (defun apply-effect ()
      (when (and document.all photoslider.filters)
	(let ((trans photoslider.filters.reveal-trans))
	  (setf (slot-value trans '*Transition)
		(floor (* (random) 23)))
	  (trans.stop)
	  (trans.apply))))

    (defun play-effect ()
      (when (and document.all photoslider.filters)
	(photoslider.filters.reveal-trans.play)))

    (defvar *which* 0)

    (defun keep-track ()
      (setf window.status
	    (+ "Image " (1+ *which*) " of " photos.length)))

    (defun backward ()
      (when (> *which* 0)
	(decf *which*)
	(apply-effect)
	(setf document.images.photoslider.src
	      (aref photos *which*))
	(play-effect)
	(keep-track)))

    (defun forward ()
      (when (< *which* (1- photos.length))
	(incf *which*)
	(apply-effect)
	(setf document.images.photoslider.src
	      (aref photos *which*))
	(play-effect)
	(keep-track)))

    (defun transport ()
      (setf window.location (aref photoslink *which*)))))


;; Web server publishing
(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(define-easy-handler (lwt-presentation-handler :uri "/presentation") ()
  (with-html
    (:html
     (:head (:title "ParenScript tutorial: 2st example"))
     (:body (:h1 "ParenScript tutorial: 2st example")
	    (:table :border 0 :cellpadding 4
		    (loop for i below 25 by 5
		       do (htm
			   (:tr :align "right"
				(loop for j from i below (+ i 5)
				   do (htm
				       (:td :bgcolor (if (oddp j)
							 "pink"
							 "green")
					    (fmt "~@R" (1+ j)))))))))))))

(define-easy-handler (tutorial1-file-handler :uri "/tutorial1.js") ()
  (tutorial1-file))

(define-easy-handler (lwt-tutorial-handler :uri "/tutorial") ()
  (tutorial1))

(define-easy-handler (slideshow-handler :uri "/slideshow") ()
  (slideshow))

(define-easy-handler (js-slideshow-handler :uri "/slideshow.js") ()
  (js-slideshow))

(add-dispatchers
 'lwt::dispatch-easy-handlers
 (create-static-file-dispatcher-and-handler "/photo1.jpg"
					    (make-pathname :name "photo1" :type "jpg"
							   :version nil :defaults *this-file*))
 (create-static-file-dispatcher-and-handler "/photo2.jpg"
					    (make-pathname :name "photo2" :type "jpg"
							   :version nil :defaults *this-file*))
 (create-static-file-dispatcher-and-handler "/photo3.jpg"
					    (make-pathname :name "photo3" :type "jpg"
							   :version nil :defaults *this-file*)))