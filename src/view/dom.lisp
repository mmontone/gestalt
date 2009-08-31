(in-package :gst.view.dom)

(defclass dom-xml-node ()
  ((node-id :accessor node-id
	    :documentation "The node's id")
   (appended-p :accessor appended-p
	       :initform nil
	       :documentation "t when the node is appended to the base tree")
   (node-id-counter :accessor node-id-counter
		    :initform 1
		    :documentation "This slot is used to generate new node ids"))
  (:documentation "This class is meant to be used as a mixin. We keep track of "))

(defun assign-subtree-ids (node parent &key (set-appended t))
  (setf (node-id node) (cons (node-id-counter parent) (node-id (parent node))))
  (incf (node-id-counter parent))
  (when set-appended
    (setf (appended-p node) t))
  (do-children (child node)
    (assign-subtree-ids child node
			:set-appended set-appended)))

(defun clear-subtree-ids (node &key (unset-appended t))
  (setf (node-id node) nil)
  (when unset-appended
    (setf (appended-p node) nil))
  (do-children (child node)
    (clear-subtree-ids child
		       :unset-appended unset-appended)))

(defun get-node-with-id (id tree)
  (labels ((%get-node-with-id (path tree)
	     (when (equalp (car path) (car (node-id tree)))
	       (if (equalp (length path) 1)
		   (return-from get-node-with-id tree)
		   (do-children (child tree)
		     (let ((next-child (cadr path)))
		       (when (equalp next-child (car (node-id child)))
			 (return-from %get-node-with-id
			   (%get-node-with-id (cdr path)
						 child)))))))))
    (%get-node-with-id (reverse id) tree)))

;----------------------
; Operations wrappers
;----------------------

(defvar *assign-ids* t "When true, node operations assign ids")

(defmethod append-child :after ((node dom-xml-node) child)
  (when (and *assign-ids* (appended-p node))
    (assign-subtree-ids child node)))

(defmethod replace-child :after ((node dom-xml-node) child replacement)
  (declare (ignore child))
  (when (and *assign-ids* (appended-p node))
    (assign-subtree-ids replacement node)))

(defmethod insert-child-after :after ((node dom-xml-node) child reference-child)
  (declare (ignore reference-child))
  (when (and *assign-ids* (appended-p node))
    (assign-subtree-ids child node)))

(defmethod insert-child-before :after ((node dom-xml-node) child reference-child)
  (declare (ignore reference-child))
  (when (and *assign-ids* (appended-p node))
    (assign-subtree-ids child node)))

(defmethod remove-child :after ((node dom-xml-node) child)
  (when (and *assign-ids* (appended-p child))
    (clear-subtree-ids child)))

(defmethod make-base-tree :after ((node dom-xml-node) &rest args)
  (declare (ignore args))
  (setf (node-id node) '(1))
  (setf (appended-p node) t)
  (do-children (child node)
    (assign-subtree-ids child node)))

(defmethod copy-xml-tree :around ((tree dom-xml-node) &rest args)
  (if (getf args :copy-ids t)
      (let ((new-tree (apply #'call-next-method tree args)))
	(setf (slot-value new-tree 'node-id)
	      (slot-value tree 'node-id))
	new-tree)
      (apply #'call-next-method tree args)))

(defmethod xml-node-equal :around ((tree1 dom-xml-node) (tree2 dom-xml-node))
  (and (call-next-method)
       (equalp (node-id tree1)
	       (node-id tree2))))

;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod xml:print-slot-with-name-p :around ((dom-xml-node dom-xml-node) name)
  (and (call-next-method)
       (not (one-of ("node-id" "node-id-counter" "appended-p") name
		    :test #'string-equal))))