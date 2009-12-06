#|
Algorithm:

+---------------+--------------+----------------+----------------+---------------+
|               | remove-child |  append-child  | replace-child  | insert-before |
+---------------+--------------+----------------+----------------+---------------+
| remove-child  |    error     |      null      | remove-child   |     null      |
+---------------+--------------+----------------+----------------+---------------+
| append-child  |    error     |       x        |       x        |       x       |
+---------------+--------------+----------------+----------------+---------------+
| replace-child |    error     |  append-child  | replace-child  | insert-before |
|               |              |       B        | AxC (ignore B) |       B       |
+---------------+--------------+----------------+----------------+---------------+
| insert-before |    error     |       x        |       x        |       x       |
+---------------+--------------+----------------+----------------+---------------+
      
|#


(in-package :gst.view.xml)

(defvar *register-modifications* t
  "When this is active, node modifications are registered in the modifications slot")

;-------------------------
; tracked-xml-node mixin
;-------------------------

(defclass tracked-xml-node ()
  ((parent-modification :accessor parent-modification
			:documentation "The modification made to the
                                        node's to add this node as a child")
   (modifications :accessor modifications
		  :initform '()
		  :documentation "Node modifications list.
                                  TODO?: hold modifications with a dlist??"))
  (:documentation
   "This class is meant to be used as a mixin (see xml-node definition).
    When *register-modifications* is active, changes to the node are
    registered in the modifications slot"))

;----------------------------------
;  Node modifications
;----------------------------------

(defclass xml-node-modification ()
  ((target :initarg :target
	   :accessor target
	   :initform (error "Provide the target node (usually the parent node)")
	   :documentation "Holds the modification's affected node"))
  (:documentation "A modification of a xml-node"))

(defclass append-child-modification (xml-node-modification)
  ((child :initarg :child
	  :accessor child
	  :initform (error "Provide the child node to append")
	  :documentation "The child node appended"))
  (:documentation "This modification means a child node was appended"))

(defclass insert-child-modification (xml-node-modification)
  ((child :initarg :child
	  :accessor child
	  :initform (error "Provide the child node to insert")
	  :documentation "The child node appended")
   (reference-position :initarg :reference-position
		       :accessor reference-position
		       :initform (error "Provide the reference position")
		       :documentation "The position in the tree we reference
                                       to position the inserted child")
   (place :initarg :place
	  :accessor place
	  :initform (error "Provide the place")
	  :documentation "Tells where to insert the child.
                          One of :after o :before (the reference child)"))
  (:documentation "This modification means a child node was appended"))

(defmethod reference-child ((mod insert-child-modification))
  (dlist::dlink-content (reference-position mod)))

(defclass replace-child-modification (xml-node-modification)
  ((child :initarg :child
	  :accessor child
	  :initform (error "Provide the child node to replace")
	  :documentation "The child node replaced")
   (replacement :initarg :replacement
		:accessor replacement
		:initform (error "Provide the replacement node")
		:documentation "The replacement node"))
  (:documentation "This modification means a child node was replaced"))

(defclass remove-child-modification (xml-node-modification)
  ((child :initarg :child
	  :accessor child
	  :initform (error "Provide the child node to remove")
	  :documentation "The child node removed"))
  (:documentation "This modification means a child node was removed"))

;----------------------------------
; Modifications related operations
;----------------------------------

(defmethod print-object ((mod append-child-modification) stream)
  (print-unreadable-object (mod stream :identity t)
    (format stream "append ~A to ~A" (child mod) (target mod))))

(defmethod print-object ((mod replace-child-modification) stream)
  (print-unreadable-object (mod stream :identity t)
    (format stream "replace ~A by ~A" (child mod) (replacement mod))))

(defmethod print-object ((mod remove-child-modification) stream)
  (print-unreadable-object (mod stream :identity t)
    (format stream "remove ~A" (child mod))))

(defmethod print-object ((mod insert-child-modification) stream)
  (print-unreadable-object (mod stream :identity t)
    (format stream "insert ~A ~A ~A"
	    (child mod)
	    (place mod)
	    (reference-child mod))))

(defmethod make-base-tree-node :after ((node tracked-xml-node) &rest args)
  (flush-modifications node))

(defun remove-from-base-tree (node)
  (flush-modifications node :recurse t)
  (make-base-tree node :value nil))

(defgeneric append-modification-p (modification)
  (:method ((mod t))
    (declare (ignore mod))
    nil)
  (:method ((modification append-child-modification))
    (declare (ignore modification))
    t))

(defgeneric insert-modification-p (modification)
  (:method ((mod t))
    (declare (ignore mod))
    nil)
  (:method ((modification insert-child-modification))
    (declare (ignore modification))
    t))

(defgeneric replace-modification-p (modification)
  (:method ((mod t))
    (declare (ignore mod))
    nil)
  (:method ((modification replace-child-modification))
    (declare (ignore modification))
    t))

(defgeneric remove-modification-p (modification)
  (:method ((mod t))
    (declare (ignore mod))
    nil)
  (:method ((modification remove-child-modification))
    (declare (ignore modification))
    t))

(defmethod modified-p ((node tracked-xml-node))
  (not (null (modifications node))))

(defun flush-modifications-down (node)
  (labels ((flush-down (node)
	     (flush-node-modifications node)
	     (do-children (child node)
	       (flush-down child))))
    (flush-down node)))
    
(defun flush-modifications (node &key recurse)
  (if recurse
      (flush-modifications-down node)
      (flush-node-modifications node)))

(defmethod flush-node-modifications ((node tracked-xml-node))
  (setf (parent-modification node) nil)
  (setf (modifications node) '()))

(defmethod add-modification ((mod xml-node-modification) (node tracked-xml-node))
  (setf (modifications node) (nconc (modifications node) (list mod))))

(defmethod remove-modification ((mod xml-node-modification) (node tracked-xml-node))
  (setf (modifications node) (delete mod (modifications node))))

(defun is-appended (node)
  (append-modification-p (parent-modification node)))

(defun is-inserted (node)
  (insert-modification-p (parent-modification node)))

(defun is-a-replacement (node)
  (replace-modification-p (parent-modification node)))

(defmethod extract-modifications ((node tracked-xml-node)
				  &key (flush nil)
				  (declare-base-tree-member-p nil))
  (prog1
      (append
       (modifications node)
       (flatten
	(collect-children (child node)
           (extract-modifications child :flush flush))))
    (progn
      (when flush
	(flush-node-modifications node))
      (when declare-base-tree-member-p
	(setf (base-tree-member-p node) t)))))

;-----------------------------------
; Wrapped xml-node operations
;-----------------------------------

(defmethod append-child :after ((node tracked-xml-node) child)
  (when (and *register-modifications* (base-tree-member-p node))
    (let ((modification (make-instance 'append-child-modification
				       :target node
				       :child child)))
      (setf (parent-modification child) modification)
      (add-modification modification node))))

(defmethod remove-child :after ((node tracked-xml-node) child)
  (when (and *register-modifications* (base-tree-member-p node))
    (cond
      ((is-appended child) (remove-modification (parent-modification child) node))
      ((is-inserted child) (remove-modification (parent-modification child) node))
      ((is-a-replacement child) (let* ((replace-mod (parent-modification child))
					(replaced-node (child replace-mod)))
				   (remove-modification replace-mod node)
				   (add-modification (make-instance 'remove-child-modification
								    :target node
								    :child replaced-node) node)))
      (t (let ((modification (make-instance 'remove-child-modification
					    :target node
					    :child child)))
	   (setf (parent-modification child) modification)
	   (add-modification modification node))))
    (remove-from-base-tree child)))

(defmethod insert-child-before :after ((node tracked-xml-node) child reference-child)
  (when (and *register-modifications* (base-tree-member-p node))
    (let ((modification (make-instance 'insert-child-modification
				       :place :before
				       :target node
				       :child child
				       :reference-position (parent-link reference-child))))
      (setf (parent-modification child) modification)
      (add-modification modification node))))

(defmethod insert-child-after :after ((node tracked-xml-node) child reference-child)
  (when (and *register-modifications* (base-tree-member-p node))
    (let ((modification (make-instance 'insert-child-modification
				       :place :after
				       :target node
				       :child child
				       :reference-position (parent-link reference-child))))
      (setf (parent-modification child) modification)
      (add-modification modification node))))

(defmethod replace-child :after ((node tracked-xml-node) child replacement)
  (when (and *register-modifications* (base-tree-member-p node))
    (cond
      ((is-appended child) (let ((append-mod (parent-modification child)))
			     (setf (child append-mod) replacement)))
      ((is-inserted child) (let ((insert-mod (parent-modification child)))
			     (setf (child insert-mod) replacement)))
      ((is-a-replacement child) (let ((replace-mod (parent-modification child)))
				  (setf (replacement replace-mod) replacement)))
      (t (let ((modification (make-instance 'replace-child-modification
					    :target node
					    :child child
					    :replacement replacement)))
	   (setf (parent-modification replacement) modification)
	   (add-modification modification node))))
    (remove-from-base-tree child)))
 
(defmethod set-attribute :after ((node tracked-xml-node) attribute value)
  (when (and *register-modifications* (base-tree-member-p node))
    (add-modification (make-instance 'set-attribute-modification
				     :target node
				     :attribute attribute
				     :value value)
		      node)))

(defmethod remove-attribute :after ((node tracked-xml-node) attribute)
  (when (and *register-modifications* (base-tree-member-p node))
    (add-modification (make-instance 'remove-attribute-modification
				     :target node
				     :attribute attribute)
		      node)))

(defmethod copy-xml-tree :around ((tree tracked-xml-node) &rest args)
  ;; disable modifications tracking when copying
  (let ((*register-modifications* nil))
    (apply #'call-next-method tree args)))


;------------------------------
;   XMLisp Glue
;------------------------------

(defmethod xml:print-slot-with-name-p :around ((tracked-xml-node tracked-xml-node) name)
  (and (call-next-method)
       (not (one-of ("modifications"
		     "parent-modification"
		     "base-tree-member-p") name
		    :test #'string-equal))))

;--------------------------------
; Modifications serialization
;--------------------------------

(defvar *serialization-outputs* nil "The available serialization outputs")

(defmacro define-serialization-output (name &rest defs)
  (let ((methods-defs
	 (loop for def in defs
	    collect (destructuring-bind (modification-type (mod stream) &rest body) def
	       (let ((modification-class
		      (ecase modification-type
			(:append 'append-child-modification)
			(:insert 'insert-child-modification)
			(:remove 'remove-child-modification)
			(:replace 'replace-child-modification))))
		 (with-unique-names (out)
		 `(defmethod serialize-modification ((,mod ,modification-class)
						     (,out (eql ,name))
						     &optional (,stream *standard-output*))
		      ,@body)))))))
    `(progn
       (pushnew ,name *serialization-outputs*)
       ,@methods-defs)))

(defun serialize-modifications (xml-node &optional (output *standard-output*)
				         &key (serialization-output :ajax)
				              (flush t))
  (let ((modifications (extract-modifications xml-node :flush flush)))
    (loop for modification in modifications
	 do (serialize-modification modification
				    serialization-output
				    output))))


;----------------------------------
;  AJAX serialization output
;----------------------------------

(define-serialization-output :ajax
    (:append (mod stream)
	     (with-html-output (stream)
	       (:append-child :target (str (node-id (target mod)))
			      (esc (print (child mod) nil))))) ;; calls print-object
  (:replace (mod stream)
	    (with-html-output (stream)
	      (:replace-child :target (str (node-id (target mod)))
			      :child (str (node-id (child mod)))
			      (esc (print (replacement mod) nil)))))
  (:insert (mod stream)
	   (with-html-output (stream)
	     (:insert-child :target (str (node-id (target mod)))
			    :reference-child (str (node-id (reference-child mod)))
			    :place (str (place mod))
			    (esc (print (replacement mod) nil)))))
  (:remove (mod stream)
	   (with-html-output (stream)
	     (:remove-child :target (str (node-id (target mod)))
			    :child (str (node-id (child mod)))))))

;-----------------------------------
; JSON serialization output
;-----------------------------------

(define-serialization-output :json
    (:append (mod stream)
	     (encode-json mod stream))
  (:replace (mod stream)
	    (encode-json mod stream))
  (:insert (mod stream)
	   (encode-json mod stream))
  (:remove (mod stream)
	   (encode-json mod stream)))

;; (defmethod serialize-modification ((modification xml-modification)
;; 				   (output-type (eql :json))
;; 				   &optional (output *standard-output*))
;;   (encode-json modification
;; 	       output))

(defmethod encode-json ((modification append-child-modification) stream)
  ;; Idea: define a special pamarenscript scaping form lisp* that scapes
  ;; its body like the lisp form, but calls encode-json to the result ;)
  (write 
   (ps
     (get-node-with-id (lisp (node-id (target modification)))))
   stream))