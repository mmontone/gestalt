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
    (base-tree-member-p :accessor base-tree-member-p
			:initform nil
			:documentation "Flag that tells whether the node belongs to the base tree or not"))
   (:documentation "An xml node"))

(defclass xml-container (xml-node)
  ((children :accessor children
	     :initform (make-dlist)))
  (:documentation "An HTML element that contains other HTML elements. Use as a mixin"))

;---------------------------
;  Macros
;---------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-children ((var node) &body body)
    `(loop for ,var in (dlist-elements (children ,node))
	do (progn ,@body)))

  (defmacro collect-children ((var node) &body body)
    `(loop for ,var in (dlist-elements (children ,node))
	collect (progn ,@body))))

;---------------------------
; XML nodes operations
;---------------------------
(defgeneric append-child (node child)
  (:documentation "Append a child node to other node")
  (:method ((node xml-container) (child xml-node))
    (assert (null (parent child)))
    (let ((parent-link (insert-tail (children node) child)))
      (setf (parent-link child) parent-link)
      (setf (parent child) node))))

(defgeneric replace-child (node child replacement)
  (:documentation "Replace a child of a node")
  (:method ((node xml-container) (child xml-node) (replacement xml-node))
    (assert (and (not (null (parent child)))
		 (eql (parent child) node)
		 (null (parent replacement))))
    (let ((link (parent-link child)))
      (setf (dlist::dlink-content link) replacement)
      (setf (parent-link replacement) link)
      (setf (parent replacement) node)
      (setf (parent-link child) nil)
      (setf (parent child) nil))))

(defgeneric remove-child (node child)
  (:documentation "Remove a child from a node")
  (:method ((node xml-container) (child xml-node))
    (assert (and (not (null (parent child)))
		 (eql (parent child) node)))
    (remove-link (children node) (parent-link child))
    (setf (parent-link child) nil)
    (setf (parent child) nil)))

(defgeneric insert-child-before (node child reference-child)
  (:documentation "Inserts a child node before the reference-child")
  (:method ((node xml-node) (child xml-node) (reference-child xml-node))
    (assert (null (parent child)))
    (assert (childp reference-child node))
    (setf (parent-link child)
	  (insert-before (children node)
			 (parent-link reference-child)
			 child))
    (setf (parent child) node)))
    
(defgeneric insert-child-after (node child reference-child)
  (:documentation "Inserts a child after the reference-child")
  (:method ((node xml-container) (child xml-node) (reference-child xml-node))
    (assert (null (parent child)))
    (assert (childp reference-child node))
    (setf (parent-link child)
	  (insert-after (children node)
			(parent-link reference-child)
			child))
    (setf (parent child) node)))
 
(defgeneric insert-child (node child place reference-child)
  (:documentation "Inserts the child :after or :before (place) the reference child")
  (:method ((node xml-container)
	    (child xml-node)
	    place
	    (reference-child xml-node))
    (case place
      (:after (insert-child-after node
				  child
				  reference-child))
      (:before (insert-child-before node
				    child
				    reference-child))
      (t (error "Provide the place (one of :before or :after)")))))

(defgeneric set-attribute (node attribute value)
  (:documentation "Set the value of a node's attribute. The primary method is just a wrapper for slot-value")
  (:method ((node xml-node) attribute value)
    (setf (slot-value node attribute) value)))

(defgeneric get-attribute (node attribute)
  (:documentation "Reads a node's attribute value")
  (:method ((node xml-node) attribute)
    (slot-value node attribute)))

(defgeneric remove-attribute (node attribute)
  (:documentation "Removes a node's attribute")
  (:method ((node xml-node) attribute)
    (set-attribute node attribute nil)))

(defgeneric make-base-tree-node (node &rest args)
  (:documentation "Declare a node to be part of the base tree")
  (:method ((node xml-node) &rest args)
    (declare (ignore args))
    (setf (base-tree-member-p node) t)))
    
(defgeneric make-base-tree (node &rest args)
  (:documentation "Declares a tree as a base one")
  (:method ((node xml-node) &rest args)
    (assert (null (parent node)))
    (apply #'make-base-tree-node node args)
    (do-children (child node)
       (apply #'make-base-tree-node node args))))

;------------------------------
; Querying
;------------------------------
(defgeneric tag-name (node)
  (:method ((xml-node xml-node))
    (class-name (class-of xml-node))))

(defun is-next (other-child child)
  "Tells if child is at the right of other-child"
  (let* ((other-link (parent-link other-child))
	 (link (dlist::dlink-next other-link)))
    (eql (dlist::dlink-content link) child)))

(defun comes-after (other-child child)
  "Tells if child is one of the nodes after other-child"
  (assert (and (not (null (parent child)))
	       (eql (parent other-child) (parent child))))
  (loop with found-first-p = nil
     for ch in (dlist-elements (children (parent child)))
     when (eql ch other-child) do (setf found-first-p t)
     when (eql ch child) do (return-from comes-after found-first-p))
  (error "This shouldn't have happened!!"))

(defun is-previous (other-child child)
  "Tells if child is at the left of other-child"
  (let* ((other-link (parent-link other-child))
	 (link (dlist::dlink-prev other-link)))
    (eql (dlist::dlink-content link) child)))

(defun comes-before (other-child child)
  "Tells if child is one of the nodes at the left of other-child"
  (let* ((other-link (parent-link other-child)))
    (loop for link = (dlist::dlink-prev other-link) then (dlist::dlink-next link)
       if (eql (dlist::dlink-content link) child)
       do (return-from comes-before t)
       finally (return nil))))

(defun childp (child parent)
  (eql (parent child) parent))

;------------------------------
; Copying and comparing
;------------------------------

(defgeneric copy-xml-tree (tree &rest args)
  (:documentation "Copy an xml tree")
  (:method ((tree xml-node) &rest args)
    (declare (ignore args))
    (let ((new-tree (make-instance (class-of tree))))
      (setf (slot-value new-tree 'base-tree-member-p)
	    (slot-value tree 'base-tree-member-p))
      new-tree)))

(defmethod copy-xml-tree ((tree xml-container) &rest args)
  (let ((new-tree (apply #'call-next-method tree args)))
    (do-children (child tree)
      (append-child new-tree (apply #'copy-xml-tree child args)))
    new-tree))

(defgeneric xml-node-equal (tree1 tree2)
  (:documentation "t when two nodes are equal")
  (:method ((tree1 xml-node) (tree2 xml-node))
    t))

(defgeneric xml-tree-equal (tree1 tree2)
  (:documentation "t when the two trees have equal structure")
  (:method ((tree1 xml-node) (tree2 xml-node))
    (and (xml-node-equal tree1 tree2)
	 (equalp (length-dlist (children tree1))
		 (length-dlist (children tree2)))
	 (reduce (lambda (acc trees)
		   (destructuring-bind (tree1 . tree2) trees
		     (and acc (xml-tree-equal tree1 tree2))))
		 (mapcar #'cons
			 (dlist:dlist-elements (children tree1))
			 (dlist:dlist-elements (children tree2)))
		 :initial-value t))))

;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod xml:print-slot-with-name-p ((xml-node xml-node) name)
  (not (one-of ("parent-link" "parent") name
	       :test #'string-equal)))

(defmethod xml:map-object ((dlist dlist) function)
  (map-dlist function dlist))

(defmethod xml:xml-printable-as-subelement-p ((dlist dlist))
  t)

(defmethod xml:add-subobject ((node xml-container) (child xml-node))
  (append-child node child))