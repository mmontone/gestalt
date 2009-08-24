(in-package :gst.util)

(defclass freevars-replacer-code-walker (cw:code-walker)
  ((replace-fn :initarg :replace-fn
	       :accessor replace-fn
	       :required t
	       :documentation "The function used to replace free variables"))
  (:metaclass required-slots-class))

(defmethod initialize-instance :after ((code-walker freevars-replacer-code-walker) &rest initargs)
  (declare (ignore initargs))
  (setf (cw::parent code-walker) cw:*standard-code-walker*)
  (cw:define-code-walker-case :freevar (code-walker form lexenv)
    (funcall (replace-fn code-walker) form lexenv)))

(defparameter *freevars-replacer*
  (make-instance 'freevars-replacer-code-walker
		 :name "freevars-replacer"
		 :replace-fn (lambda (var lexenv) (intern (concatenate 'string (symbol-name var) "-FREEVAR")))))


