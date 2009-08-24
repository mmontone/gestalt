(in-package :gst.view.dom)

(defclass dom-xml-node ()
  ((node-id :accessor node-id
	    :initform 1
	    :documentation "The node's id")
   (children-counter :accessor children-counter
		     :initform 1
		     :documentation "This slot is used to generate new node ids"))
  (:documentation "This class is meant to be used as a mixin. We keep track of "))

;----------------------
; Operations wrappers
;----------------------

(defmethod :after append-child ((node dom-xml-node) child)
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

(defmethod :after replace-child ((node dom-xml-node) child)
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

(defmethod :after insert-child-after ((node dom-xml-node) child reference-child)
  (declare (ignore reference-child))
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

(defmethod :after insert-child-before ((node dom-xml-node) child reference-child)
  (declare (ignore reference-child))
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))