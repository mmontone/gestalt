;; Macros for standard DSL creation
;; This is meant to work and be used by dataflow, transactional memory and references

(in-package :gstutils)


;; (defmacro defunstm (name args &rest body)
;;   `(progn
;;      (setf (gethash ',name *stmfuns*) t)
;;      (defun ,name ,args (stm ,@body))))

(defmacro make-let (name make-binding)
  "This macro creates let style macros.
   Let style macros are supposed to introduce new, fresh, bindings.

   Parameters:
      name : The name of the macro to be generated. The name of the macro has the form let-<name>.
      make-binding : A function that takes the binding-name and the binding-value
                     and produces a 'creation list' (see examples)
"
  (let ((macro-name (intern (concatenate 'string "LET-" (symbol-name name)))))
  `(defmacro ,macro-name (bindings &rest body)
     (let ((new-bindings
	    (loop for binding in bindings
	       collect (list (car binding) (funcall ,make-binding (car binding) (cadr binding))))))
       `(let
	    ,new-bindings
	  ,@body)))))

(defmacro make-using (name access-var &optional name-binding)
  "This macro creates 'using' ling macros.
   Contrary to let style macros, using like macros take existing bindings and
   access them in a special way in its body.

   Parameters:
      name : The name of the macro to be generated. The name of the macro has the form using-<name>.
      access-var : A function that takes a binding and creates a list that access it (see examples)"

  (let ((macro-name (intern (concatenate 'string "USING-" (symbol-name name))))
	(name-binding (if (null name-binding)
			  `(lambda (binding-name)
			     (concatenate 'string binding-name "-" (symbol-name ',name) "-"))
			  name-binding)))
    `(defmacro ,macro-name (vars &body body)
       (let
	   ((bindings '()))
	 `(let
	      ,(loop for var in vars
		  for var-binding = (gensym (funcall ,name-binding (symbol-name var)))
	     collect (list
		      (progn
		       (push (cons var var-binding) bindings)
		       var-binding)
		      var))
       (symbol-macrolet
	   ,(loop for binding in bindings
		  collect (list (car binding)
				(funcall ,access-var (cdr binding))))
	 ,@body))))))


(defmacro make-with (name &optional let-macro using-macro)
  "This macro creates with style macros.
   with style macros are a combination of both let and using style macros as they introduce new bindings and access them in a special way in its body.
   This implements the generated macro using a let-macro and a using-macro.

  Parameters:
     name : The name of the generated macro. The name of the macro has the form with-<name>.
     let-macro : The name of the let-macro to use.
     using-macro : The name of the using-macro to use. "
  
  (let
      ((let-macro (if (null let-macro)
		      (intern (concatenate 'string "LET-" (symbol-name name)))
		      let-macro))
       (using-macro (if (null using-macro)
			(intern (concatenate 'string "USING-" (symbol-name name)))
		      using-macro))
       (macro-name (intern (concatenate 'string "WITH-" (symbol-name name)))))
    `(defmacro ,macro-name (bindings &rest body)
       (let
	   ((let-macro ',let-macro)
	    (using-macro ',using-macro))
	 `(,let-macro
	   ,bindings
	   (,using-macro
	    ,(mapcar #'car bindings)
	    ,@body))))))

"TODO: rewrite functions above with (loop for
                                         collect
                                          and do ..) syntax"

"TODO: write a free-variables macro maker (see stm and df macros)"