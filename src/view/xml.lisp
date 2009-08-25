(in-package :gst.view.xml)

;-----------------------------
;   XML classes
;-----------------------------

(defclass xml-node (xml:xml-serializer)
   ((parent :initarg :parent
	    :accessor parent
	    :initform nil
	    :documentation "The node's parent")
    (parent-link :initarg :parent-link
		 :accessor parent-link
		 :documentation "The node's link in parent's children (the dlist's dlink)")
    ;; (attributes :initarg :attributes
    ;; 		:accessor attributes
    ;; 		:initform (make-hash-table :test #'equalp)
    ;; 		:documentation "A table with the node's attributes")
    )
   (:documentation "An xml node"))

(defclass xml-container (xml-node)
  ((children :accessor children
	     :initform (make-dlist)))
  (:documentation "An HTML element that contains other HTML elements. Use as a mixin"))

;---------------------------
; XML nodes operations
;---------------------------
(defmethod append-child ((node xml-container) (child xml-node))
  (let ((parent-link (insert-tail (children node) child)))
    (setf (parent-link child) parent-link)
    (setf (parent child) node)))

(defmethod replace-child ((node xml-container) (child xml-node)
			                       (new-child xml-node))
  (setf (dlist::dlink-content (parent-link child)) new-child)
  (setf (parent-link child) nil))

(defmethod remove-child ((node xml-container) (child xml-node))
  (remove-link (children node) (parent-link child))
  (setf (parent-link child) nil))

(defmethod insert-child-before ((node xml-node)
				(child xml-node)
				(reference-child xml-node))
  (insert-before (children node)
		 (parent-link reference-child)
		 child))

(defmethod insert-child-after ((node xml-container)
			       (child xml-node)
			       (reference-child xml-node))
  (insert-after (children node)
		(parent-link reference-child)
		child))
 
(defmethod insert-child ((node xml-container)
			 (child xml-node)
			 &key (after nil after-p) (before nil before-p))
  (cond
    (after-p (insert-child-after node child after))
    (before-p (insert-child-before node child before))
    (t (error "Provide the :before or :after node"))))

;; (defmethod set-attribute ((node xml-node) attribute value)
;;   (setf (gethash attribute (attributes node)) value))

;; (defmethod get-attribute ((node xml-node) attribute)
;;   (gethash attribute (attributes node)))

;; (defmethod remove-attribute ((node xml-node) attribute)
;;   (remhash attribute (attributes node)))

;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod xml:print-slot-with-name-p ((xml-node xml-node) name)
  (not (one-of ("parent-link" "parent") name
	       :test #'string-equal)))

(defmethod tag-name ((xml-node xml-node))
  (class-name (class-of xml-node)))

(defmethod xml:map-object ((dlist dlist) function)
  (map-dlist function dlist))

(defmethod xml:xml-printable-as-subelement-p ((dlist dlist))
  t)

(defmethod xml:add-subobject ((node xml-container)
			  (child xml-node))
  (append-child node child))