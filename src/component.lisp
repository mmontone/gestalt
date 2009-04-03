(in-package :gestalt)

;; Pensamiento: muchas de las variables son dependientes de la aplicación. Por lo tanto, es lógico que muchas de ellas sean implementadas como slots de la clase application. Sin embargo, también es posible desacoplar determinadas variables, por ejemplo, declarandolas en una tabla de hash aparte y proveyendo sintaxis conveniente del estilo:
;; (defappvar page-renderer :documentation "The page renderer")
;; (with-appvar (page-renderer) ...)

(defclass component ()
  ())

(defclass application ()
  ((view-creator :documentation "The view creator")
   (page-renderer :documentation "The page renderer")
   (translator :documentation "The language translator"))
  (:documentation "The application class"))

(defclass window ()
  ((view :documentation "The window's view")
   (url-manager :documentation "The URL manager"))
  (:documentation "A browser's window"))

(defclass dom-xml-node ()
  ((children :documentation "The node's children")
   (parent :documentation "The node's parent")
   (parent-position :documentation "Node's position in parent's children")
   (next-node :documentation "The node to the right??")
   (tag-name :initarg :tag-name :initform "" :documentation "The node's tag name")
   (attributes :initarg :attributes :initform '() :documentation "The node's attributes"))
  (:documentation "A DOM XML node. We are always working with DOM nodes (is this true, for every interface??), so this is the top class"))

(defclass xml-node (dom-xml-node)
  ((controller :documentation "The controller (what's this??)")
   (templates :documentation "The templates (explain further why they are here)")
   (containers :documentation "Maybe explain somewhere what the continariners are")
   (children-by-id :documentation "Node's children mapped by id")
   (css-classes :documentation "CSS classes that apply to this node")
   (cache :documentation "The cache (explain what's this)"))
(:documentation "An XML node"))

(defclass xml-node-modifications-tracker (xml-node)
  ((to-flush :documentation "The things to flush (explain further)")
   (registering :documentation "Weather we are registering or not (explain better of course)")))


(defclass html-container (xml-node-modifications-tracker)
  ()
  (:documentation "An HTML container"))

(defclass xml-node-modification ()
  ((target :initarg :target)))

(defmethod will-flush ((mod xml-node-modification))
  t)

(defmethod add-child-modification ((mod xml-node-modification) pos child-mod)
  (error "subclass responsibility"))

(defmethod remove-child-modification ((mod xml-node-modification) pos)
  (error "subclass responsibility"))

(defmethod print-tree ((mod xml-node-modification))
  (format nil "~A" (class-of mod)))


;;; (defmethod render-js-response-command ((mod xml-modification))
;;;   ;; TODO: use html macros and parenscript here
;;;   (format *rendering-output* "~s" "<script>var str = \"<ajax>")
;;;   (format *rendering-output* "~s" (xmlize (render-ajax-response-command mod)))
;;;   (format *rendering-output* "~s" "</ajax>\"parWin.all_updatePage(str, (parWin.str2html(str)).childNodes);</script>")
;;;   (flush-rendering-output))

;; The above should be translated to something like this:

(defmethod render-js-response-command ((mod xml-modification))
  (let ((ajax-js ""))
    (with-html-ouput-to-string (ajax-js)
      (:ajax (str (xmlize (render-ajax-response-command mod)))))
    (with-html-output (*rendering-output*)
      (:script (js (let ((str (str ajax-js)))
		     (par-win.all-update-page str (par-win.str2html str).child-nodes)))))))

(define-condition flush-rendering-output-condition ()
  ())

(defun flush-rendering-output ()
  "This function should send the *rendering-output* buffer to the browser and clean it (we could raise a flush-rendering-output-condition to alter the control flow)"
  (signal 'flush-rendering-output-condition))

(defmacro with-rendering-output ((output form) &body body)
  `(let ((*rendering-output* nil))
     (handler-case ,form
       (flush-rendering-output-condition
	   (let ((,output *rendering-output*))
	     ,@body)))))

;; Example:
;; (with-rendering-output (out (render-app *app*))
;;      (http-send out))

(defclass bookmark-xml-node-modification (xml-node-modification)
  ((hash :initarg :hash :documentation "The bookmark hash"))
  (:documentation "A bookmark node modification"))

(defmethod render-ajax-response-command ((mod bookmark-xml-node-modification))
  (with-html-output-to-string (html)
    (:bookmark :hash (str (hash mod)))))

(defmethod render-js-response-command ((mod bookmark-response-command))
  (with-html-output (*rendering-output*)
    (:script (js (par-win.do-bookmark (str (hash mod))))))
  (flush-rendering-output))

(defclass replace-child-xml-node-modification (xml-node-modification)
  ((child :accessor child :initarg :child)
   (replacement :accessor replacement :initarg :replace-by))
  (:documentation "A node's replacement modification"))

(defmethod render-ajax-response-command ((mod replace-child-xml-node-modification))
  (with-html-output-to-string (html)
    (:repn :id (str (id (child mod)))
	   (str (render (replacement mod))))))

(defmethod apply-replacement ((mod replace-child-xml-node-modification) elem)
  (setf (replacement mod) elem))

(defmethod render-js-response-command ((md replace-child-xml-node-modification))
  (with-html-output-to-string (s)
    (:script
     (js (par-win.do-repn (par-win.document.get-element-by-id (str (id (child mod))) (str (xmlize (render (replacement mod))))))))))

(defclass null-xml-node-modification (xml-node-modification)
  ()
  (:documentation "Explain why we need this!! (smells like a hack)"))

(defmethod render-ajax-response-command ((mod null-xml-node-modification))
  "Do nothing"
  )

(defmethod will-flush ((mod null-xml-node-modification))
  "The null modification doesn't flush (whatever that means)"
  nil)

(defclass child-modifications-xml-node-modification (xml-node-modification)
  ((modifications :accessor modifications :initarg '()))
  (:documentation "Means that children have been modified"))

(defmethod render-ajax-response-command ((mod child-modifications-xml-node-modification))
  (let ((xml "")) 
    (loop for child-mod in (modifications mod)
	 do (setf xml (concatenate 'string xml (render child-mod))))))

(defmethod add-child-modification ((mod child-modifications-xml-node-modification) pos child-mod)
  (setf (gethash pos (modifications mod)) child-mod))

(defmethod remove-child-modification ((mod child-modifications-xml-node-modification) pos)
  (remhash pos (modifications mod)))

(defmethod print-tree ((mod child-modifications-xml-node-modification))
  (let
      ((ret (concatenate 'string (print (class-of mod) nil) "{")))
    (maphash
     (lambda (pos child-mod)
       (setf ret (concatenate 'string (string pos) ":" (print-tree child-mod))))
     (modifications mod))
    ret))

;; Let's see how with-hash-table-iterator works!!

(defmethod print-tree ((mod child-modifications-xml-node-modification))
  (let
      ((ret (concatenate 'string (print (class-of mod) nil) "{")))
    (with-hash-table-iterator (iterator (modifications mod))
      (multiple-value-bind (pos child-mod) (iterator)
	(setf ret (concatenate 'string (string pos) ":" (print-tree child-mod)))))
    ret))

;; Let's see how loop works!!
(defmethod print-tree ((mod child-modifications-xml-node-modification))
  (let
      ((ret (concatenate 'string (print (class-of mod) nil) "{")))
    (loop for pos being the hash-keys of (modifications mod)
       using (hash-value child-mod)
        do (setf ret (concatenate 'string (string pos) ":" (print-tree child-mod))))
    ret))

;; The above three methods should all do the same. Leave only one.

(defmethod render-js-response-command ((mod child-modifications-xml-node-modification))
  (loop for child-mod being the hash-values of (modifications mod)
       do (render-js-response-command child-mod)))

(defclass insert-before-xml-node-modification (xml-node-modification)
  ((old-node :initarg old-node :accessor oldnode :initform (error "Provide the old node"))
   (new-node :initarg new-node :accessor new-node :initform (error "Provide the new node"))))

(defmethod render-ajax-response-command ((mod xml-node-modification))
  (with-html-output (*rendering-output*)
    (:insert :id (str (id (old-node mod)))
	     (str (render (new-node mod))))))

(defmethod apply-replace ((mod xml-node-modification) new-node)
  (setf (new-node mod) new-node))

(defmethod render-js-response-command ((mod xml-node-modification))
  (with-html-output (*rendering-output*)
    (:script
     (js (par-win.do-insert (par-win.document.get-element-by-id (str (id (child mod))))))))
  (flush-rendering-ouput))

(defclass append-child-xml-node-modification (xml-node-modification)
  ((child :initarg :child))
  (:documentation "There's an append to a child modification"))

(defmethod render-ajax-response-command ((mod append-child-xml-node-modification))
  
  )



(defmethod create-view ((wnd window))
  (setf (url-manager wnd) (make-instance 'url-manager :window wnd))
  (setf (view wnd) (make-instance 'xml-node-modifications-tracker))
  (set-attribute (view wnd) "id" (id wnd))
  (let
      ((container (make-instance 'html-container)))
    (init-page (page-renderer *app*) wnd)
    (setf (slot-value wnd to-flush) (make-instance 'child-modifications-xml-node-modification))))

(defmethod set-root-component ((app application))
  (error "subclass-responsibility"))

(defmethod restart ((app application))
  (restart *session*))

(defmethod initial-render ((app application))
  (let (()))
  )

