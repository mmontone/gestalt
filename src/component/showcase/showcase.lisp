(in-package :gestalt)

(defparameter *demos*
  (flet ((source (filename)
	   (asdf:system-relative-pathname :gestalt (format nil "src/component/showcase/~A" filename))))
    `((counters . (:label "Counters"
			  :component counters-showcase
			  :source  ,(source "counters.lisp")))
      (lists . (:label "Lists"
		       :component lists-showcase
		       :source ,(source "lists.lisp")))
      (tabs . (:label "Tabs"
		      :component tabs-showcase
		      :source ,(source "tabs.lisp")))
      (embedded . (:label "Embedded"
			  :component showcase
			  :source ,(source "showcase.lisp"))))))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

;; The showcase component

(defcomponent showcase ()
  ((selected-demo :serialize t
		  :initform 'counters
		  :accessor selected-demo)
   (body :component t
	 :initform (let ((demo (cdr (assoc 'counters *demos*))))
		     (make-instance 'tabs-component
				    :tabs `((demo . ,(make-instance (getf demo :component)))
					    (source . ,(make-instance 'source-viewer :source (getf demo :source))))))
	 :accessor body)))


(define-renderer ((showcase showcase))
  (htm (:ul :class "demos"
	    (loop for demo in *demos*
		 do
		 (htm (:li (:a :href (action-link switch-to-demo showcase (first demo))
			       (str (getf (cdr demo) :label)))))))
       (:div :class "demo"
	     (render (body showcase)))))

(define-unserialization showcase
  (let ((showcase (call-next-method)))
    (switch-to-demo showcase (selected-demo showcase))
    showcase))
	    
(define-action switch-to-demo (showcase demo)
  (let ((demo (cdr (assoc demo *demos*))))
    (setf (body showcase)
	  (make-instance 'tabs-component
			 :tabs `((demo . ,(make-instance (getf demo :component)))
				 (source . ,(make-instance 'source-viewer :source (getf demo :source))))))))

(defcomponent source-viewer ()
  ((source :initarg :source
	   :initform nil
	   :accessor source))
  (:render (source-viewer)
	   (htm (:div (str (file-string (source source-viewer)))))))

(defun showcase ()
  (start-application (make-instance 'application :root (make-instance 'showcase))
		     :port 9090))
