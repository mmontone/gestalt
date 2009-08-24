(in-package :gst.view.xml)

;-----------------------------
;   XML classes
;-----------------------------

(defclass xml-node (xml-serializer)
   ((id :initarg :id
	:accessor node-id
	:initform nil
	:documentation "A string that uniquely identifies the node in the tree")
    (parent :initarg :parent
	    :accessor parent
	    :documentation "The node's parent")
    (parent-link :initarg :parent-link
		 :accessor parent-link
		 :documentation "The node's link in parent's children (the dlist's dlink)")
    (attributes :initarg :attributes
		:accessor attributes
		:initform (make-hash-table :type #'equalp)
		:documentation "A table with the node's attributes"))
   (:documentation "An xml node"))

(defclass xml-container ()
  ((children :accessor children
	     :initform (make-dlist)))
  (:documentation "An HTML element that contains other HTML elements. Use as a mixin"))

;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod print-slot-with-name-p ((xml-node xml-node) name)
  (not (one-of ("parent-link" "parent") name
	       :test #'string-equal)))

(defmethod tag-name ((xml-node xml-node))
  (class-name (class-of xml-node)))

(defmethod map-object ((dlist dlist) function)
  (map-dlist function dlist))

(defmethod xml-printable-as-subelement-p ((dlist dlist))
  t)

(defmethod add-subobject ((xml-container xml-container)
			  (xml-node xml-node))
  (let ((parent-link (insert-tail (children xml-container)
				  xml-node)))
    (setf (parent-link xml-node) parent-link)
    (setf (parent xml-node) xml-container)
    (update-node-ids xml-container)))

;---------------------------
; XML nodes operations
;---------------------------
(defmethod append-child ((node xml-node) child)
  (let ((dlink (insert-tail (children node) child)))
    (setf (parent-link child) dlink)))

(defmethod replace-child ((node xml-node) child new-child)
  (setf (dlist::dlink-content (parent-link child)) new-child))

(defmethod remove-child ((node xml-node) child)
  (remove-link (parent-link child) (children node)))

(defmethod insert-child-before ((node xml-node)
				child reference-child)
  (insert-before (children node)
		 (parent-link reference-child)
		 child))

(defmethod insert-child-after ((node xml-node)
			       child reference-child)
  (insert-after (children node)
		(parent-link reference-child)
		child))
 
(defmethod insert-child ((node xml-node)
			 child &key (after nil after-p)
			            (before nil before-p))
  (cond
    (after-p (insert-child-after node child after))
    (before-p (insert-child-before node child before))
    (t (error "Provide the :before or :after node"))))

(defmethod set-attribute ((node xml-node) attribute value)
  (setf (gethash attribute (attributes node)) value))

(defmethod get-attribute ((node xml-node) attribute)
  (gethash attribute (attributes node)))

(defmethod remove-attribute ((node xml-node) attribute)
  (remhash attribute (attributes node)))

(defmethod print-object ((node xml-node) stream)
  (print-unreadable-object (node stream :identity t :type t)
    (format stream "~A" (tag-name node))))
