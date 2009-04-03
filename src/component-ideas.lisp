;;; (defmacro addlast (x list)
;;;   `(setf ,list (append ,list (list ,x))))

;;; (defmacro doplist ((key val) plist &body body)
;;;   `(let ((.plist-tail. ,plist) ,key ,val)
;;;      (loop (when (null .plist-tail.) (return nil))
;;;            (setq ,key (pop .plist-tail.))
;;;            (when (null .plist-tail.)
;;;              (error "malformed plist, odd number of elements"))
;;;            (setq ,val (pop .plist-tail.))
;;;            (progn ,@body))))

;;; (defun convert-slotdef (slotdef)
;;;   (let ((new-def (list (car slotdef))))
;;;     (doplist (k v) (cdr slotdef)
;;;       (addlast k new-def)
;;;       (addlast 
;;;        (if (equal k :initform)
;;; 	   `(make-instance 'dfvaluecell :value ,v)
;;; 	   v)
;;;        new-def))
;;;     new-def))

;;; (defmacro defcomponent (name direct-superclasses direct-slots &rest options)
;;;   `(progn
;;;      (defclass ,name ,(cons 'component direct-superclasses)
;;;      ,(loop for slotdef in direct-slots
;;; 	 collect (convert-slotdef slotdef))
;;;      ,@options)
;;;      ,@(loop for slotdef in direct-slots
;;; 	  collect `(defmethod (setf ,(car slotdef)) (new-value (component ,name))
;;; 		     (setf (value (slot-value component ',name)) new-value)))))

;;; (defclass model-class (standard-class) ;; We need a combination of persistent-class and dataflow-class   ala monad-transformers
  
  
;;;   )

;;; (defmacro defmodel (name direct-superclasses direct-slots &rest options)
;;;   `(with-rucksack *gs-store*
;;;      (with-transaction
;;; 	 (defclass ,name ,direct-superclasses ,direct-slots ,@(append options '((:metaclass model-class)))))))