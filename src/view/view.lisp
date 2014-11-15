(in-package :gst.view)

(defclass view-node (xml-node tracked-xml-node dom-xml-node)
  ((template
    :accessor template
    :initform nil
    :documentation "The template the node was created from")
   (print-cache
    :accessor print-cache
    :initform nil
    :documentation
    "We hold a cached printed representation of
     this node in this variable. It is cleaned
     whenever the node changes")
   (encoded-node-id :accessor encoded-node-id
		    :initform ""
		    :documentation "The encoded node id (for dom browser handling)")
   (handler :initarg :handler
	    :accessor handler
	    :initform nil
	    :documentation "The view handler")
   (controller :initarg :controller
	       :accessor controller
	       :initform nil
	       :documentation "The associated controller"))
  (:documentation "A node of the view is an xml-node
                   with changes tracked"))

(defmethod view-controller ((view-node view-node))
  (flet ((%view-controller (node)
	   (if (controller node)
	       (controller node)
	       (if (null (parent node))
		   (error "~A has no attached controller" view-node)
		   (%view-controller (parent node))))))
    (%view-controller view-node)))

(defclass standard-view-context ()
  ((parent :initarg :parent
	   :accessor parent
	   :initform nil
	   )))

(defclass global-view-context (standard-view-context)
  ())

(defclass template-view-context (standard-view-context)
  ((template
    :initarg :template
    :accessor template))
  )

(defparameter *global-view-context* (make-instance 'global-view-context))

;; (defmethod add-component :after :view
;;   ((parent component) slot (child component))
;;   (let ((place-holder
;; 	 (find-place-holder (view component)
;; 			    slot))
;; 	(view (assign-view child :context
;; 			   (make-instance 'template-context
;; 					  :template (template (view component))
;; 					  :parent *global-view-context*))))
;;     (clear-place-holder place-holder)
;;     (loop for v in view
;; 	 do (add-child place-holder v))))

;; (defmethod assign-view ((component component) &key (context *global-view-context*))
;;   (let ((view (make-view context component)))
;;     (setf (view component) view)))

;; (defmethod make-view ((context global-view-context)
;; 		      (component component))
;;   (let ((templates-for-class
;; 	 (find-all-templates-for-class component)))
;;     (if (emptyp templates-for-class)
;; 	(error "Could not find a view for ~A in context ~A"
;; 	       component context)
;; 	())))



(defclass view-container (view-node xml-container)
  ())

(defclass a (gst.view.html:a view-node)
  ())

(defclass div (gst.view.html:div view-container)
  ())

(defclass p (gst.view.html:p view-node)
  ())

(defclass special-element (view-container)
  ()
  (:documentation "A special (and non-printable) element"))

(defclass updater (special-element)
  ((updater :initarg :updater
	    :initform (error "Provide the updater")
	    :accessor updater
	    ))
  )

(defmethod empty-node ((xml-node xml-node))
  )

(defmethod empty-node ((xml-container xml-container))
  (loop for child in (children xml-container)
       do (remove-child child xml-container)))

(defvar *debug* nil)

(defmethod print-object ((view-node special-element) stream)
  (if *debug*
      (call-next-method)
      (loop for child in (children view-node)
	 do (print-object child stream))))

(defmethod populate-template-instance ((view-node view-node) node)
  (setf (slot-value view-node 'template)
	node))

(defmethod populate-template-instance ((view-node a) node)
  (call-next-method)
  (setf (slot-value view-node 'href)
	(slot-value node 'gst.view.html::href))
  (setf (slot-value view-node 'xml:content)
	(slot-value node 'xml:content)))

(defmethod populate-template-instance ((view-node div) node)
  (call-next-method)
  (setf (slot-value view-node 'children)
	(slot-value node 'gst.view.html::children)))

(defmethod populate-template-instance ((view-node p) node)
  (call-next-method)
  (setf (slot-value view-node 'xml:content)
	(slot-value node 'xml:content)))

(defmethod make-template-instance ((node xml-node))
  (let ((view-node
	 (make-instance
	  (intern (string-upcase
		   (xml:xml-tag-name-string node))
		  (find-package :gst.view)))))
    (populate-template-instance view-node node)
    view-node))

(defmethod make-template-instance ((update update))
  (let ((function-name (gensym "UPDATER-FUNCTION-")))
    (compile function-name (read-from-string (content update)))
    (make-instance 'updater
		   :updater (symbol-function function-name))))

(defmethod make-template-instance ((container container))
  (let ((function-name (gensym "UPDATER-FUNCTION-")))
    (compile function-name '(lambda (view controller)

			     )
    (make-instance 'updater
		   :updater (symbol-function function-name)))))

(defmethod make-template-instance ((loop *loop))
  )

(defmethod make-template-instance ((if *if))
  )

;----------------------
; Operations wrappers
;----------------------

;; node-id encoding
(defmethod (setf node-id) :after (value (node view-node))
  (declare (ignore value))
  (setf (encoded-node-id node)
	(encode-node-id (node-id node))))


(defvar *root-view* nil "The view of the system")

(defmethod make-base-tree ((node view-node) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf *root-view* node))

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
	     (do-children (child node)
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

(defmethod append-child :after ((node view-container) child)
  (declare (ignore child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod replace-child :after ((node view-container) child new-child)
  (declare (ignore child new-child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod insert-child-after :after ((node view-container) child reference-child)
  (declare (ignore child reference-child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod insert-child-before :after ((node view-container) child reference-child)
  (declare (ignore child reference-child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defmethod remove-child :after ((node view-container) child)
  (declare (ignore child))
  (when *flush-print-cache*
    (flush-print-cache node)))

(defvar *node-id-delimiter* #\:)

(defun node-id-to-string (node-id)
  (foldl (lambda (pos str)
	   (concatenate 'string
			(string *node-id-delimiter*)
			(princ-to-string pos)
			str))
	 node-id ""))

(defun string-to-node-id (string)
  (nreverse
   (mapcar #'parse-integer
	  (cdr
	   (split-sequence:split-sequence
	    *node-id-delimiter*
	    string)))))

(defun encode-node-id (node-id)
  (usb8-array-to-base64-string
   (encrypt
    (string-to-octets (node-id-to-string node-id)))
   :uri t))

(defun decode-node-id (encoded-node-id)
  (string-to-node-id
   (octets-to-string
    (decrypt
     (base64-string-to-usb8-array
      encoded-node-id :uri t)))))

;---------------------------
; Applying modifications
;---------------------------

(defmethod apply-modifications (modifications (tree xml-node))
  (loop for modification in modifications
        do (apply-modification modification tree)))

(defmethod apply-modification ((mod append-child-modification) tree)
  (let ((target
 	 (get-node-with-id (node-id (target mod)) tree)))
    (assert target nil "Node with id ~A not found in ~A when applying ~A"
	    (node-id (target mod)) tree mod)
    (append-child target (copy-xml-tree (child mod)))))

(defmethod apply-modification ((mod insert-child-modification) tree)
  (let ((target
 	 (get-node-with-id (node-id (target mod)) tree))
 	(reference-child
 	 (get-node-with-id (node-id (reference-child mod)) tree)))
    (assert target nil "~A not found when applying ~A" (target mod) mod)
    (assert reference-child nil "~A not found when applying ~A"
	    (reference-child mod) mod)
    (insert-child target
                  (copy-xml-tree (child mod))
		  (place mod)
		  reference-child)))

(defmethod apply-modification ((mod replace-child-modification) tree)
  (let ((target
 	 (get-node-with-id (node-id (target mod)) tree))
	(child (get-node-with-id (node-id (child mod)) tree)))
    (assert target nil "~A not found when applying ~A" (target mod) mod)
    (replace-child target child (copy-xml-tree (replacement mod)))))

(defmethod apply-modification ((mod remove-child-modification) tree)
  (let ((target
 	 (get-node-with-id (node-id (target mod)) tree))
	(child (get-node-with-id (node-id (child mod)) tree)))
    (assert target nil "~A not found when applying ~A" (target mod) mod)
    (assert child nil "~A not found when applying ~A" (child mod) mod)
    (remove-child target child)))

(defmethod apply-modification :around ((mod xml-node-modification)
				       (tree tracked-xml-node))
  ; disable modifications tracking when applying modifications
  (let ((*register-modifications* nil))
    (call-next-method)))

(defmethod apply-modification :around ((mod xml-node-modification)
				       (tree dom-xml-node))
  ; disable id assignation when applying modifications
  (let ((*assign-ids* nil))
    (call-next-method)))

;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod xml:print-slot-with-name-p ((view-node view-node) name)
  (and (call-next-method)
       (not (one-of ("print-cache"
		     "handler"
		     "controller") name
		    :test #'string-equal))))