(defmacro wlambda (args &rest body)
  (labels
      ((map-tree (f g tree)
	 (if (atom tree)
	     (funcall g tree)
	     (funcall f (car tree)
		      (mapcar (lambda (arg) (map-tree f g arg)) (cdr tree)))))
       (register-possible-weakref (possible-weakref table)
	 (when (and (symbolp possible-weakref) (equal (position #\@ (string possible-weakref)) 0))
	   (setf (gethash possible-weakref table) possible-weakref)))
       (list-weakrefs (body)
	   (let ((weakrefs (make-hash-table :test #'equal))
		 (returns '()))
	     (mapcar
	      (lambda (sentence)
		(map-tree
		 (lambda (atom rest)
		   (declare (ignore rest))
		   (register-possible-weakref atom weakrefs)
		   atom)
		 (lambda (atom)
		   (register-possible-weakref atom weakrefs)
		   atom)
		 sentence))
		body)
	     (maphash
	      (lambda (k v)
		(declare (ignore v))
		(push k returns))
	      weakrefs)
	     returns)))
    (let ((weakrefs-makes '()))
      (loop for weakref in (list-weakrefs body)
	 do (push (cons weakref (gensym (string weakref))) weakrefs-makes))
      `(let ,(loop for (weakref . gensym) in weakrefs-makes
		  collect (list gensym `(make-weakref ,(intern (remove #\@ (string weakref))))))
	 (lambda ,args
	   (symbol-macrolet
	       ,(loop for (weakref . gensym) in weakrefs-makes
		   collect `(,weakref (weakref-value ,gensym :error-on-nil t)))
	     ,@body))))))

;; Other possible design
;; use (declare (weak var1 var2 ... varn))

(wlambda (button)
	 (declare (weak this that))
	 (set-color this 'red)
	 (setf (color that) 'blue)
	 (print (message this)))

;; is transformed into the following:

(LET ((#:THIS928 (MAKE-WEAKREF THIS)) (#:THAT929 (MAKE-WEAKREF THAT)))
  (LAMBDA (BUTTON)
    (SYMBOL-MACROLET ((THIS (WLAMBDA-WEAKREF-VALUE #:THIS928))
                      (THAT (WLAMBDA-WEAKREF-VALUE #:THAT929)))
      (SET-COLOR (WLAMBDA-WEAKREF-VALUE #:THIS928) 'RED)
      (LET* ((#:G931 (WLAMBDA-WEAKREF-VALUE #:THAT929)))
        (MULTIPLE-VALUE-BIND
	      (#:G930)
            'BLUE
          (FUNCALL #'(SETF COLOR) #:G930 #:G931)))
      (PRINT (MESSAGE (WLAMBDA-WEAKREF-VALUE #:THIS928))))))

(defmacro wlambda (args &rest body)
  (if (equal (string (caar body)) "DECLARE")
      (let ((weak-refs (cdr (cadr (car body))))
	    (table (make-hash-table :test #'equal)))
	`(let
	    ,(loop for weak-ref in weak-refs
		collect (let
			    ((sym (gensym (string weak-ref))))
			  (setf (gethash weak-ref table) sym)
			  (list sym `(make-weakref ,weak-ref))))
	   (lambda ,args
	     (symbol-macrolet
		 ,(loop for weak-ref in weak-refs
		     collect `(,weak-ref (wlambda-weakref-value ,(gethash weak-ref table))))
	       ,@(cdr body)))))
      `(lambda ,args ,@body)))

;; Let's try to improve it with weak references checking

(defmacro wlambda (args &rest body)
  (if (equal (string (caar body)) "DECLARE")
      (let ((weak-refs (cdr (cadr (car body))))
	    (table (make-hash-table :test #'equal))
	    (evaled-weakrefs (make-hash-table :test #'equal)))
	`(let
	    ,(loop for weak-ref in weak-refs
		collect (let
			    ((sym (gensym (string weak-ref))))
			  (setf (gethash weak-ref table) sym)
			  (list sym `(make-weakref ,weak-ref))))
	   (lambda ,args
	     (let ,(loop for weak-ref in weak-refs
			collect (let ((evaled-ref-sym (gensym (string weak-ref))))
				  (setf (gethash weak-ref evaled-weakrefs) evaled-ref-sym)
				  (list evaled-ref-sym nil)))
	       (when (and ,@(loop for weak-ref in weak-refs
			       collect
			       (let ((target (gensym "TARGET"))
				     (found (gensym "FOUND")))
				 `(multiple-value-bind (,target ,found) (weakref-value ,(gethash weak-ref table))
				    (setf ,(gethash weak-ref evaled-weakrefs) ,target)
				    ,found))))
		 (symbol-macrolet
		     ,(loop for weak-ref in weak-refs
			 collect (list weak-ref (gethash weak-ref evaled-weakrefs)))
		 ,@(cdr body)))))))
	`(lambda ,args ,@body)))

;; And now we have:

(LET ((#:THIS1073 (MAKE-WEAKREF THIS)) (#:THAT1074 (MAKE-WEAKREF THAT)))
  (LAMBDA (BUTTON)
    (LET ((#:THIS1075 NIL) (#:THAT1076 NIL))
      (IF
       (IF
        (MULTIPLE-VALUE-BIND
	      (#:TARGET1077 #:FOUND1078)
            (WEAKREF-VALUE #:THIS1073)
          (SETQ #:THIS1075 #:TARGET1077)
          #:FOUND1078)
        (MULTIPLE-VALUE-BIND
	      (#:TARGET1079 #:FOUND1080)
            (WEAKREF-VALUE #:THAT1074)
          (SETQ #:THAT1076 #:TARGET1079)
          #:FOUND1080)
        NIL)
       (PROGN
	 (SYMBOL-MACROLET ((THIS #:THIS1075) (THAT #:THAT1076))
	   (SET-COLOR #:THIS1075 'RED)
	   (LET* ((#:G1082 #:THAT1076))
	     (MULTIPLE-VALUE-BIND
		   (#:G1081)
		 'BLUE
	       (FUNCALL #'(SETF COLOR) #:G1081 #:G1082)))
	   (PRINT (MESSAGE #:THIS1075))))
       NIL))))

