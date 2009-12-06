(in-package :gst.view.test)

(def-suite view-test-suite)

(in-suite view-test-suite)

(defun run-tests ()
  (run 'view-test-suite))

;; basic tests

(test append-child-test
  (let* ((node (make-instance 'xml-container))
	 (child1 (make-instance 'xml-node))
	 (child2 (make-instance 'xml-node)))
    (append-child node child1)
    (is (childp child1 node))
    ;; test the order in the tree
    (append-child node child2)
    (is (and (childp child2 node)
	     (comes-after child1 child2)))
    ;; test the error when trying to append an already appended node
    (signals error
      (append-child node child1))
    ;; test the order again
    (remove-child node child1)
    (append-child node child1)
    (is (and (childp child1 node)
	     (comes-after child2 child1)))))

(test remove-child-test
  (let* ((node (make-instance 'xml-container))
	 (child1 (make-instance 'xml-node)))
    (append-child node child1)
    (remove-child node child1)
    ;; test dettaching
    (is (not (childp child1 node)))
    (is (dlist:null-dlist (children node)))
    ;; test error signaling when trying to remove an object that is not in the node
    (signals error
      (remove-child node child1))))
  
(test insert-child-test
  (let* ((node (make-instance 'xml-container))
	 (child1 (make-instance 'xml-node))
	 (child2 (make-instance 'xml-node))
	 (child3 (make-instance 'xml-node)))
    (append-child node child1)
    (insert-child node child2 :after child1)
    ;; test order
    (is (and (childp child2 node)
	     (is-next child1 child2)))
    (insert-child node child3 :before child1)
    (is (and (childp child3 node)
	     (is-previous child1 child3)))
    ;; shouldn't insert an already inserted child
    (signals error
      (insert-child node child2 :before child1))
    (remove-child node child2)
    ;; shouldn't insert if the reference-child is not one of the children
    (remove-child node child1)
    (signals error
      (insert-child node child2 :after child1))
    ;; shouldn't insert if the place is not correct
    (append-child node child1)
    (signals error
      (insert-child node child2 :blah-blah child1))
    (signals error
      (insert-child node child2 nil child1))))

(test replace-child-test
  (let* ((node (make-instance 'xml-container))
	 (child1 (make-instance 'xml-node))
	 (child2 (make-instance 'xml-node))
	 (child3 (make-instance 'xml-node))
	 (child4 (make-instance 'xml-node)))
    (append-child node child1)
    ;; fails when trying to replace a non-child node
    (signals error
      (replace-child node child2 child4))
    (append-child node child2)
    (append-child node child3)
    ;; test order
    (replace-child node child2 child4)
    (is (and (equalp (dlist:length-dlist (children node)) 3)
	     (childp child4 node)
	     (not (childp child2 node))
	     (is-next child1 child4)
	     (is-previous child3 child4)))))

;; tracking test

(defclass tracked-node (xml-container tracked-xml-node dom-xml-node)
  ())

(defmethod xml:print-slot-with-name-p :wrap-around ((node tracked-node) name)
  (or (string-equal "node-id" name)
      (call-next-method)))

(test extract-modifications-test  ; test flush and make-base-tree-member-p
  (let ((node (make-instance 'tracked-node))
	(child1 (make-instance 'tracked-node))
	(child2 (make-instance 'tracked-node)))
    (append-child node child1)
    (append-child node child2)
    (is (null (extract-modifications node))))
  (let ((node (make-instance 'tracked-node))
	(child1 (make-instance 'tracked-node))
	(child2 (make-instance 'tracked-node)))
    (make-base-tree node)
    (append-child node child1)
    (append-child node child2)
    (is (not (null (extract-modifications node))))))

;; the following tests compare trees applying the modifications
(test track-append-child-test
  (let* ((node (make-instance 'tracked-node))
	 (node-copy (progn (make-base-tree node)
			   (copy-xml-tree node)))
	 (child1 (make-instance 'tracked-node))
	 (child2 (make-instance 'tracked-node)))
    (make-base-tree node-copy)
    (append-child node child1)
    (append-child node child2)
    (let ((mods (extract-modifications node :flush t)))
      (gst.view::apply-modifications mods node-copy))
    (is (xml-tree-equal node node-copy))))
  
(test track-insert-child-test
  (let* ((node (make-instance 'tracked-node))
	 (node-copy (progn (make-base-tree node)
			   (copy-xml-tree node)))
	 (child1 (make-instance 'tracked-node))
	 (child2 (make-instance 'tracked-node))
	 (child3 (make-instance 'tracked-node)))
    (make-base-tree node-copy)
    (append-child node child1)
    (insert-child node child2 :after child1)
    (insert-child node child3 :after child2)
    (let ((mods (extract-modifications node :flush t)))
      (gst.view::apply-modifications mods node-copy))
    (is (xml-tree-equal node node-copy))))

(test track-replace-child-test
  (let* ((node (make-instance 'tracked-node))
	 (node-copy (progn (make-base-tree node)
			   (copy-xml-tree node)))
	 (child1 (make-instance 'tracked-node))
	 (child2 (make-instance 'tracked-node))
	 (child3 (make-instance 'tracked-node)))
    (make-base-tree node-copy)
    (append-child node child1)
    (insert-child node child2 :after child1)
    (replace-child node child1 child3)
    (let ((mods (extract-modifications node :flush t)))
      (gst.view::apply-modifications mods node-copy))
    (is (xml-tree-equal node node-copy))))

(test track-remove-child-test
  (let* ((node (make-instance 'tracked-node))
	 (node-copy (progn (make-base-tree node)
			   (copy-xml-tree node)))
	 (child1 (make-instance 'tracked-node))
	 (child2 (make-instance 'tracked-node))
	 (child3 (make-instance 'tracked-node)))
    (make-base-tree node-copy)
    (append-child node child1)
    (insert-child node child2 :after child1)
    (remove-child node child2)
    (let ((mods (extract-modifications node :flush t)))
      (gst.view::apply-modifications mods node-copy))
    (is (xml-tree-equal node node-copy))))

;; dom tests

(defun fold-xml-tree (f value tree)
  (if (null tree)
      value
      (funcall f tree
	       (collect-children (child tree)
				 (fold-xml-tree f value child)))))

(defun repeated-id (tree)
  (let ((ids (make-hash-table :test #'equalp)))
    (labels ((%repeated-id (tree)
	       (if (null tree)
		   nil
		   (multiple-value-bind (id found-p)
		       (gethash (encoded-node-id tree) ids)
		     (progn
		       (if found-p
			   (return-from repeated-id t)
			   (setf (gethash (encoded-node-id tree) ids) id))
		       (do-children (child tree)
			 (%repeated-id child)))))))
      (%repeated-id tree))))
		   

(test get-node-with-id-test
  (is nil "WRITE THIS TEST"))

;; view tests
(test print-cache-test
  (is nil "WRITE THIS TEST"))

(test id-encoding-test
  (is nil "WRITE THIS TEST"))

;; (defclass person ()
;;   ((name
;;     :initarg :name
;;     :accessor name
;;     :initform (error "Provide the person's name"))
;;    (lastname
;;     :initarg :lastname
;;     :accessor lastname
;;     :initform (error "Provide the person's lastname")))
;;   (:metaclass dataflow:dataflow-class)
;;   (:documentation "A simple person model object"))

;; (let ((person (make-instance 'person
;; 			     :name "Mariano"
;; 			     :lastname "Montone")))
;;   (dataflow:with-df-slots (name lastname) person
;;     (swank:inspect-in-emacs name)))


;; (defclass person-viewer (model-component)
;;   ((name :component t)
;;    (lastname :component t))
;;   (:metaclass standard-component-class))

;; (defclass initialize-instance :after ((person-viewer person-viewer) &rest initargs)
;;   (dataflow:with-df-slots (name lastname) (model person-viewer)
;;     (gst.util:with-object (person-viewer)
;;       (add-component 'name
;; 		     (make-instance 'text-widget :model name))
;;       (add-component 'lastname
;; 		     (make-instance 'text-widget :model lastname)))))