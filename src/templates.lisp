(defclass xml-node-modifications-tracker (xml-node)
  ((to-flush :accessor to-flush)
   (registering :accessor registering :initform nil))
  )

(defmethod add-child-modification ((tracker xml-node-modifications-tracker) pos mod)
  (if (will-flush tracker)
      (add-child-modification (target (to-flush tracker)) pos mod)
      ; else
      (let ((new-mod (make-instance 'child-modifications-xml-node-modification :target tracker)))
	(setf (target (to-flush tracker)) new-mode)
	(when (parent-node tracker)
	  (add-child-modification (parent-position tracker) new-mod)))))

(defmethod remove-child-modification ((tracker xml-node-modifications-tracker) pos)
  (remove-child-modification (target (to-flush tracker))))

(defmethod flush-modifications ((tracker xml-node-modifications-tracker))
  (setf (target (to-flush tracker)) (make-instance 'null-xml-node-modification :target tracker)))

(defmethod render-non-echo ((tracker xml-node-modifications-tracker))
  (setf (registering tracker) t)
  (call-next-method))

(defmacro iwhen (condition (&rest body)))

;; idea: implicit lexical scoped vars
(with-lexical-scope
    (setf @my-lex-var)  ;; @ is a reader macro
    )

;; o con macro-let
(with-lexical-scope
    (setf (@ my-lex-var))
    )
;; deberia traducir a

(let ((my-lex-var-gensym))
  (setf my-lex-var-gensym)
  )


(defmethod append-child ((tracker xml-node-modifications-tracker) child)
  (if (registering tracker)
      (let ((append-chlid-mod))
	(setf append-child-mod (make-instance 'append-child-xml-node-modification :target tracker :child child))
	(setf (target (to-flush child)) append-child-mod)
	(prog1
	    (call-next-method)
	  (add-child-modification (parent-position child) append-child-mod)))
      (call-next-method)))


  
(defmethod will-flush ((tracker xml-node-modifications-tracker))
  (not (is-null (to-flush tracker))))

(defmethod is-null ((mod t))
  nil)

(defmethod is-null ((mod null-xml-node-modification))
  t)

(defmethod will-flush-node ((tracker xml-node-modifications-tracker))
  (let ((to-flush (target (to-flush tracker))))
    (and (not (is-null to-flush)) (not (is-child-modification to-flush)))))

(defmethod is-child-modification ((mod t))
  nil
  )

(defmethod is-child-modification ((mod child-modifications-xml-node-modification))
  t
  )
  
(defmethod replace-child ((tracker xml-node-modifications-tracker) &key new-child old-child)
  "We don't want modifications on the new-child to be taken into account by the page renderer"
  (when (registering tracker)
    (if (will-flush-node old-child)
	(let ((to-flush (target (to-flush old-child))))
	  (apply-replace to-flush new-child)
	  (setf (target (to-flush new-child)) to-flush))
	;; else
	(let ((replace-child (make-instance 'replace-child-xml-node-modification
					    :target tracker
					    :old-child old-child
					    :new-child new-child)))
	  (setf (target (to-flush new-child)) replace-child)
	  (add-child-modification tracker (parent-position old-child) replace-child))))
  (call-next-method))

(defmethod remove-child ((tracker xml-node-modifications-tracker) child)
  (when (registering tracker)
    (if (will-flush-node child)
	(let ((to-flush (target (to-flush child))))
	  (if (is-replace-child-modification to-flush)
	      (add-child-modification (parent-position child)
				      (make-instance 'remove-child-xml-node-modification
						     :target tracker
						     :child child))
	      ;; else
	      (remove-child-modification tracker (parent-position child))))
	;; else
	(add-child-modification tracker (parent-position child)
				(make-instance 'remove-child-xml-node-modification :target tracker :child child))))
  (call-next-method))

(defmethod redraw ((tracker xml-node-modifications-tracker))
  (when (and (regsitering tracker) (parent-node tracker))
    (replace-child (parent-node tracker) :old-child tracker
		   :new-child (shallow-copy tracker))))

(defmethod set-attribute ((tracker xml-node-modifications-tracker) attribute value)
  (when (registering tracker)
    (add-child-modification tracker attribute (make-instance 'set-attribute-xml-node-modification :target tracker :attribute attribute :value value)))
  (call-next-method))

(defmethod insert-before ((tracker xml-node-modifications-tracker) &key old new)
  (when (registering tracker)
    (setf (target (to-flush new)) (make-instance 'insert-before-xml-node-modification :target tracker :old old :new new)))
  (prog1
      (call-next-method)
    (when (registering tracker)
      (add-child-modification tracker (parent-position new) new))))

(defmethod remove-attribute ((tracker xml-node-modifications-tracker) attribute)
  (when (registering tracker)
    (add-child-modification tracker attribute (make-instance 'remove-attribute-xml-node-modification :target tracker :attribute attribute)))
  (call-next-method))

(defmethod print-tree ((tracker xml-node-modifications-tracker))
  (let ((to-flush (target (to-flush tracker))))
    (format nil "~s (~n){~s}"
	    (write-to-string (class-of tracker))
	    (object-id tracker)
	    (print-tree to-flush))))

(defun object-id (obj)
  #+ccl (format nil "~x" (%address-of (make-instance 'standard-object)))
  #+sbcl (kernel::lisp-object-address))

(defmethod render-ajax-response-command ((tracker xml-node-modifications-tracker))
  (if (will-flush tracker)
      (let ((to-flush (target (to-flush tracker))))
	(render-ajax-response-command to-flush)))
  "")

(defmethod render-response-command ((tracker xml-node-modifications-tracker))
  (if (will-flush tracker)
      (let ((to-flush (target (to-flush tracker))))
	(render-js-response-command to-flush))
      ""))

(defmethod handler ((tracker xml-node-modifications-tracker))
  (get-attribute "handler" tracker))
 

(defclass html-container (xml-node-modifications-tracker)
  ())

(defmethod initialize-instance ((container html-container) &rest initargs)
  (find :id)
  )

(defmethod render ((container html-container) &optional (stream nil))
  (get-attribute container :tagname :if-absent *default-tag*)
  
  )

(defmethod real-id ((container html-container))
  (setf (attribute container "fake-id")
	(format nil "~s:~s:~s"
	  (if (parent-node container)
	      (setf id (real-id container))
	      "")
	  (attribute container "class")
	  (attribute container "simple-id"))))

(defmethod id ((container html-container))
  (real-id container))

(defmethod is-container ((container html-container))
  t)

(defmethod is-container ((container t))
  nil)

(defmethod is-container-for-component ((container html-container) comp)
  (subtypep (class-of comp) (class-of container)))

(defmethod get-class ((container html-container))
  (attribute container "class"))

(defclass xml-variable (xml-node-modifications-tracker)
  ())

(defmethod real-id ((var xml-variable))
  (attribute var "id"))