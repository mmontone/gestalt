(in-package :gst.util)

(defcategory ref)
(defcategory none)

#|
(start-sender 'ref
  (stream-sender :location *error-output*)  
  :category-spec '(ref)
  :output-spec '(message))

(start-sender 'none
  (stream-sender :location *error-output*)  
  :category-spec '(none)
  :output-spec '(message))

|#

(defclass ref ()
  ((name :initarg :name
	 :accessor name
	 :required t :error-msg "Provide the reference name")
   (value :initarg :value
	  :accessor value))
  (:metaclass required-slots-class))

(defmethod print-object ((ref ref) stream)
  (print-unreadable-object (ref stream :type t :identity t)
    (format stream "name: ~A value: ~A" (name ref) (value ref))))

(defgeneric refp (object)
  (:method ((o t)) nil)
  (:method ((o ref)) t))


(make-using refs (lambda (binding) `(value ,binding)))

(make-let refs (lambda (binding value)
		  `(make-instance 'ref
				  :name ,(symbol-name binding)
				  :value ,value)))
(make-with refs)		  

(defmacro rdefun (name args &body body)
  `(defun ,name ,args
     (using-refs ,args
       ,@body)))