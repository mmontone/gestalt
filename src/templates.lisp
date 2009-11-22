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

#|

Templates ideas:

The idea is to have template-combinations, and some other options. Besides, we may want to make the templating engine extensible.

So, for example:

<template class="<component-class>"
          combination="<template-combination>">
   ...
   <next-template/>
</template>

Examples:

We may want to have :above (equivalent to :before), :below (equivalent to :after) and :around. Besides, we can embed other templates (in the case of subclassing or :around method combinations, for example) using the special XML element <next-template/>

<template class="object-editor"
          combination="above">
   <container id="object-name"/>
</template>

<template class="object-editor"
          combination="below">
   <container id="accept-action"/>
   <container id="cancel-action"/>
</template>

Or the equivalent to the above>

<template class="object-editor"
          combination="around">
   <container id="object-name"/>
   <next-template/>
   <container id="accept-action"/>
   <container id="cancel-action"/>
</template>

Besides, it also makes sense to use <next-template/> outside and :around template-combination. It is the equivalent of call-next-method

The following model (imagine the corresponding components...):
(defclass person ()
  ((name :accessor name
	 :initarg :name
	 :initform (error "Provide the name"))
   (lastname :accessor lastname
	     :initarg :lastname
	     :initform (error "Provide the lastname"))))

(defclass employee ()
  ((company :initarg :company
	    :accessor company))))

<template class="person-viewer">
  <container id="name"/>
  <container id="lastname"/>
</template>

<template class="employee-viewer">
   <next-template/>
   <container id="company"/>
</template>


We should also make the template system extensible. On the one hand, it should be possible to implement
new template-combinations. On the other hand, the template system should be extensible by means of options too. So, for example, we should be able to implement layered templates extending the options engine:

<template class="person-viewer"
          layer="my-layer">
  <container id="name"/>
  <container id="lastname"/>
</template>

For example, let's consider the "promotions" problem. A product promotion is available to certain users for a period of time. Let's try to model it with context layers. So, we have a promotions-layer. When it is active, we want a special promotion component to be displayed on the main-page. So we have:


(defclass main-page ()
  ((banner :component t
	   :initform (make-instance 'label :text "Hello!!. This is the main page")))
  (:metaclass component-class))

(defclass promotion-widget ()
  ((product :initarg :product
	    :reader product))
  (:metaclass component-class))

(contextl:deflayer promotion-layer ()
  ())

;; In the promotion-layer, the main-page displays the promotion-widget
(defclass main-page ()
  ((promotion-widget :component t
		     :initform (make-instance 'promotion-widget :product *product*)))
  (:metaclass component-class)
  (:layer promotion-layer))

<template class="main-page">
  <container id="banner"/>
</template>

;; We place the promotion-widget at the top, when the promotions-layer is active. Note the interaction between templates combinations and template options. That should be configurable throgh a MOP. For example, how layered templates behave in presence of template combinations.
<template class="main-page"
          combination="above"
          layer="promotion-layer">
  <container id="promotion-widget"/>
</template>

<template class="promotion-widget">
  ...
</template>

And the code sketch for activating the layers:

(defun begin-user-session ()
  (dynamic-wind
    (if (active-promotions-for-user-p *user*)
	(contextl:ensure-active-layer 'promotions-layer)
	(contextl:ensure-inactive-layer 'promotions-layer))
    (proceed
     (go-on))))

We could define a volatile-layer to improve the code:

(deflayer volatile-layer ()
  ()
  (:documentation "This kind of layers are volatile. That means, they are active for some period.
                   We have to implement the valid-layer-p method"))

(defgeneric valid-layer-p (layer &optional period)
  (:documentation "Tells whether a volatile-layer is valid in a period of time")
  (:method ((layer volatile-layer) &optional period)
    (declare (ignore layer period))
    ;; not valid by default
    nil))

(deflayer period-layer (volatile-layer)
  ((from-date-time :initarg :from
		   :reader from-date-time
		   :initform (error "Supply the from date-time"))
   (to-date-time :initarg :to
		 :reader to-date-time))
  (:documentation "A volatile-layer that is valid for a period of time"))

(defmethod valid-layer-p (layer &optional (period (date-time-now)))
  (in-period-p period (cons (from-date-time layer)
			    (to-date-time layer))))

(defmacro with-volatile-layers (layers &body body)
  (with-unique-names (valid-layers invalid-layers)
    `(let (,valid-layers ,invalid-layers)
       (loop for layer in ,layers
	    if (valid-layer-p (find-layer layer))
	      do (push layer ,valid-layers)
	    else do (push layer ,invalid-layers))
        (call-with-active-layers ,valid-layers           ; we need to implement call-with-active-layers in contextl
	  (lambda ()
	    (call-with-inactive-layers ,invalid-layers   ; we need to implement call-with-inactive-layers in contextl
		(lambda ()		       
	           ,@body)))))))
	  
(defun begin-user-session ()
  (with-volatile-layers ('promotions-layer)
    (go-on)))
  
	     
Finally, it is not clear to me whether the following is correct or not, but we could give more controls to templates throw some calculation, although I think EVERY calculation should be in the controller, so...

We could have some expressions that expand to dataflow cells:

<template class="person-viewer">
  <container id="name"/>
  <container id="lastname"/>
  <div class="age">
    <formula f="(+1 (age comp))"/>
  </div>
</template>

(+1 (age comp)) should expand to something like:

(make-formula :formula (lambda ()
			 (+1 (age comp)))) and compiled in an environment where comp makes sense




|#