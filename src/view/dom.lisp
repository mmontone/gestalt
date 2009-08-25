(in-package :gst.view.dom)

(defclass dom-xml-node ()
  ((node-id :accessor node-id
	    :initform '(1)
	    :documentation "The node's id")
   (children-counter :accessor children-counter
		     :initform 1
		     :documentation "This slot is used to generate new node ids"))
  (:documentation "This class is meant to be used as a mixin. We keep track of "))

;----------------------
; Operations wrappers
;----------------------

(defmethod append-child :after ((node dom-xml-node) child)
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

(defmethod replace-child :after ((node dom-xml-node) child new-child)
  (declare (ignore child))
  (setf (node-id new-child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

(defmethod insert-child-after :after ((node dom-xml-node) child reference-child)
  (declare (ignore reference-child))
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

(defmethod insert-child-before :after((node dom-xml-node) child reference-child)
  (declare (ignore reference-child))
  (setf (node-id child) (cons (children-counter node) (node-id node)))
  (incf (children-counter node)))

;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod xml:print-slot-with-name-p :around ((dom-xml-node dom-xml-node) name)
  (and (call-next-method)
       (not (one-of ("node-id" "children-counter") name
		    :test #'string-equal))))
