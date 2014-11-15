(in-package :build-tree)

(defclass node ()
  ((tagname :accessor tagname :initarg :tagname)
   (attributes :accessor attributes :initarg :attributes)
   (children :accessor children :initform '())
   (parent :accessor parent :initarg :parent)))

(defclass value-node (node)
  ((value :accessor value :initarg :value)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":~A" (tagname node))))

(defmethod print-object ((node value-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream ":~A value: ~A" (tagname node) (value node))))

(defvar *node-classes* (make-hash-table :test #'equalp))

(defmacro defnode (tagname superclasses slots &rest options)
  `(progn
     (defclass ,tagname ,superclasses ,slots ,@options)
     (setf (gethash ',tagname *node-classes*) t)))

(defun node-class-p (name)
  (multiple-value-bind (val foundp) (gethash name *node-classes*)
      (declare (ignore val))
      foundp))

(defmacro ematch (expr &rest clauses)
  (let ((res (gensym)))
    `(let ((,res (match ,expr ,@clauses)))
       (when (not ,res)
	 (error "Didn't match!!"))
       ,res)))

(defmethod add-child ((node node) child)
  (push child (children node))
  (setf (parent child) node))

(defun children-list-p (list)
  "It can be a literal list of children or a
   chdn form"
  (and (listp (car list))
       (or (keywordp (caar list))
	   (equalp 'chdn (caar list))
	   (equalp 'chd (caar list)))))

(defun build-tree (form)
  (cond
    ((atom form) form)
    ((keywordp (car form))
     (let ((tagname (intern (symbol-name (car form)))))
       (multiple-value-bind (attributes value-or-children)
	   (consume-attributes (cdr form))
	 (let ((attr-list `(list ,@(loop for (attr . value) in attributes
							   collect `(cons ,attr ,value)))))
	   (if (children-list-p value-or-children)
	       ;; They are children
	       (let ((node (gensym "NODE")))
		 `(let ((,node ,(if (node-class-p tagname)
				    `(make-instance ',tagname
						    :attributes ,attr-list)
				    `(make-instance 'node
						    :tagname ',tagname
						    :attributes ,attr-list))))
		    ,@(cond
		       ((equalp 'chdn (caar value-or-children))
			(let ((child (gensym "CHILD")))
			  `((loop for ,child in ,(cadar value-or-children)
			       do (add-child ,node ,child)))))
		       ((equalp 'chd (caar value-or-children))
			`((add-child ,node ,(cadar value-or-children))))
			(t
			 (let ((children-code (mapcar #'build-tree value-or-children)))
			   (loop for child-code in children-code
			      collect `(add-child ,node ,child-code)))))
		    ,node))
	       ;; Its a value-node
	       (if (node-class-p tagname)
		   `(make-instance ',tagname
				   :value ,(process-operation (car value-or-children))
				   :attributes ,attr-list)
		   `(make-instance 'value-node
				   :tagname ',tagname
				   :value ,(process-operation (car value-or-children))
				   :attributes ,attr-list)))))))
    ((equalp 'str (car form))
     `(format nil "~s" ,(cadr form)))
    ((equalp 'esc (car form))
     (cadr form))
    (t (error "Malformed tree ~A" form))))

(defun process-operation (form)
  (if (atom form)
      form
      (case (car form)
	(str `(format nil "~s" ,(cadr form)))
	(esc (cadr form))
	(t form))))

(defmacro tree (form)
  (build-tree form))

(defun consume-attributes (form)
  "Consumes the attributes in form. Applies process-operation to the values.
   Returns: the attributes and the corresonding values in cons pairs, and the rest of the form"
  (labels
      ((read-attribute (form)
	 (if (not (null form))
	   (if (keywordp (car form))
	     ;; There's an attribute
	     (let ((attribute (symbol-name (car form))))
	       (read-value (cdr form)
			(lambda (value form)
			  (multiple-value-bind (attrs formcdr) (read-attribute form)
			      (values (cons (cons attribute value) attrs) formcdr)))
			(lambda ()
			  (error "Attribute value missing in ~A for ~A" form attribute))))
	     ;; No attribute, return the body
	     (values nil form))
	   ;; Else, there are no attributes; we assume the end of the
	   ;; attributes list
	   (values nil form)))
       (read-value (form cont error-cont)
	 (when (null form)
	   (funcall error-cont))
	 (funcall cont (process-operation (car form)) (cdr form))))
    (read-attribute form)))