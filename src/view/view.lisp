(in-package :gst.view)

(defclass view-node (xml-tracked-node dom-xml-node xml-node)
  ((print-cache
    :accessor print-cache
    :documentation "We hold a cached printed representation of this node in this variable. It is cleaned whenever the node changes")
   (encoded-node-id :accessor encoded-node-id
		    :documentation "The encoded node id (for dom browser handling)")
   (handler :initarg :handler
	    :accessor handler
	    :initform (error "Provide the handler")
	    :documentation "The view handler")
   (controller :initarg :controller
	       :accessor controller
	       :initform (error "Provide the controller")
	       :documentation "The associated controller"))
  (:documentation "A node of the view is an xml-node with changes tracked"))

;----------------------
; Operations wrappers
;----------------------

;; node-id encoding
(defmethod (setf node-id) :after (value (node view-node))
  (declare (ignore value))
  (setf (encoded-node-id node) (encode-node-id (node-id node))))

;; printing caching
(defmethod print-object :around ((node view-node) stream)
  (when (not (print-cache node))
    (let ((aux-stream (make-string-output-stream)))
      (funcall #'call-next-method node aux-stream)
      (setf (print-cache node) (get-output-stream-string aux-stream))))
  (format stream "~A" (print-cache node)))

(defvar *flush-print-cache* t)

(defun flush-local-print-cache (node)
  (when node
    (setf (print-cache node) nil)
    (flush-local-print-cache (parent node))))

(defun flush-all-print-cache (node)
  (labels ((find-root (node)
	     (if (null (parent node))
		 node
		 (find-root (parent node))))
	   (flush-down (node)
	     (setf (print-cache node) nil)
	     (loop for child in (children node)
		  (flush-down child))))
    (flush-down (find-root node))))
    
(defun flush-print-cache (node &key all)
  (if all
      (flush-all-print-cache node)
      (flush-local-print-cache node)))

(defmacro flushing-in-the-end ((node) &body body)
  "This macro is for making modifications to the tree
postponing the print-cache flush to the end to avoid performance overhead"
  `(progn
     (let ((*flush-print-cache* nil))
       ,@body)
     (flush-print-cache ,node :all t)))

(defmethod append-child :after ((node view-node) child)
  (declare (ignore child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod replace-child :after ((node view-node) child new-child)
  (declare (ignore child new-child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod insert-child-after :after ((node view-node) child reference-child)
  (declare (ignore child reference-child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod insert-child-before :after ((node view-node) child reference-child)
  (declare (ignore child reference-child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod remove-child :after ((node view-node) child)
  (declare (ignore child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defvar *node-id-delimiter* #\:)

(defun node-id-to-string (node-id)
  (reduce (lambda (count str)
	    (concatenate 'string (string count)
			 (string *node-id-delimiter*)
			 str))
	  node-id
	  :initial-value ""))

(defun string-to-node-id (string)
  (reduce (lambda (char list)
	    (cons (parse-integer char)
		  list))
	  (split-sequence:split-sequence *node-id-delimiter* string)
	  :initial-value '()))

(defun encode-node-id (node-id)
  (usb8-array-to-base64-string
   (encrypt
    (string-to-octets (node-id-to-string node-id)))
   :uri t))

(defun decode-node-id (encoded-node-id)
  (string-to-node-id
   (octects-to-string
    (decrypt
     (base64-string-to-usb8-array encoded-node-id)))))