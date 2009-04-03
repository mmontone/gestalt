(in-package :gstutils)

(defmacro defalias (fname &rest aliases)
  (let ((setf-args '()))
    
    `(progn
       (setf
	,@(loop for alias in aliases
	     do (setf setf-args (nconc setf-args (list `(symbol-function ',alias) `(function ,fname))))
	     finally (return setf-args)))
       ',aliases)))
    

;; Portable weak-refs

(defun make-weakref (object)
  #+sbcl(make-weak-pointer object)
  #-sbcl(error "make-weakref not implemented on this lisp"))

(defalias make-weakref make-wref mk-wref)

(defun weakref-value (weakref)
  #+sbcl(weak-pointer-value weakref)
  #-sbcl(error "weakref-value not implemented on this lisp"))

(defalias weakref-value wref-value)


;; Free variables manipulation
;; See dataflow package for examples

(defun list-lambda-list-vars (lambda-list)
  "Lists the vars or a lambda function arguments.
TODO: make this portable. Uses sb-int:parse-lambda-list at the moment"
  
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
				 morep more-context more-count)
      (sb-int:parse-lambda-list lambda-list)
    (declare (ignore keyp allowp auxp aux morep more-context more-count))
  (append
   required
   (mapcar (lambda (pair) (car pair)) optional)
   (mapcar (lambda (maybe-pair)
	     (if (consp maybe-pair)
		 (car maybe-pair)
		 maybe-pair)) keys)
   (when restp
     (list rest)))))


(defun replace-freevars (lexenv form replace-fn &optional (skip-form nil))
  "Takes a form and a replace function and applies it to the form's free variables and replace them by the function's result. It takes a lexical environment; a list of local variable names.
Returns a new form with the replacements applied.
  Takes into account lambda, let, let*, labels, flet, symbol-macrolet.
Ignores bindings in block, return-from, go, quote, throw.
TODO: skip-form doesn't seem to be used and lexenv should be portable.
Change the signature to:
(defun replace-freevars (form replace-fn &optional (lexenv nil) (skip-forms nil)))
TODO: rewrite with a pattern matching style/library (i.e. cl-match or fare-matcher) and use some unit test library to test
"
  
  (when (or (keywordp form)
	  (constantp form))
    (return-from replace-freevars form))
  (if (symbolp form)
      ;; May be a free variable
      (if (member form lexenv)
	  ;; Lexical-variable
	  form
	  ;; Free-var
	  (funcall replace-fn form))
      ;; It's not a var
      (progn
	(when (and (not (null skip-form)) (equal (cadr form) skip-form))
	  (return-from replace-freevars form))
	(case (car form)
	  (lambda (let ((args (list-lambda-list-vars (cadr form)))
		   (body (cddr form)))
	       (cons (car form)
		   (cons
		    (cadr form)
		    (mapcar (lambda (body-form)
			      (replace-freevars (append args lexenv)
				     body-form
				     replace-fn))
			    body)))))
	(let (let* ((new-lexenv '())
		    (bindings (mapcar (lambda (binding)
					(let ((new-binding (list (car binding)
								 (replace-freevars lexenv
										   (cadr binding)
										   replace-fn))))
					  (push (car binding) new-lexenv)
					new-binding))
				      (cadr form))))
	       (cons (car form)
		     (cons
		      bindings
		      (mapcar (lambda (body-form)
				(replace-freevars (append new-lexenv lexenv)
						 body-form
						 replace-fn))
			      (cddr form))))))
	(let* (let*
		  ((new-lexenv lexenv)
		   (bindings (mapcar (lambda (binding)
				       (let ((new-binding
					      (list (car binding)
						    (replace-freevars new-lexenv
								      (cadr binding)
								      replace-fn))))
					 (push (car binding) new-lexenv)
					 new-binding))
				     (cadr form))))
		(cons (car form)
		      (cons
		       bindings
		       (mapcar (lambda (body-form)
				 (replace-freevars new-lexenv body-form replace-fn))
			       (cddr form))))))
	(flet (let* ((new-lexenv '())
		    (bindings (mapcar (lambda (binding)
					(let* ((local-lexenv (append lexenv
								     (list-lambda-list-vars (cadr binding))))
					       (new-binding (cons (car binding)
								 (cons (cadr binding)
								       (mapcar (lambda (body-form)
										 (replace-freevars
										  local-lexenv
										  body-form
										  replace-fn))
									       (cddr binding))))))
					  (push (car binding) new-lexenv)
					  new-binding))
				      (cadr form))))
		(cons (car form)
		      (cons
		       bindings
		       (mapcar (lambda (body-form)
				 (replace-freevars (append new-lexenv lexenv)
						   body-form
						   replace-fn))
			       (cddr form))))))
	  (labels (let* ((new-lexenv '())
			 (bindings (mapcar (lambda (binding)
					   (let* ((local-lexenv (append lexenv
									(list-lambda-list-vars (cadr binding))
									(list (car binding)))))
					     (push (car binding) new-lexenv)
					     (cons (car binding)
						   (cons (cadr binding)
							 (mapcar (lambda (body-form)
								   (replace-freevars
								    (append local-lexenv new-lexenv)
								    body-form
								    replace-fn))
								 (cddr binding))))))
					   (cadr form))))
		    (cons (car form)
			  (cons
			   bindings
			   (mapcar (lambda (body-form)
				     (replace-freevars (append lexenv new-lexenv)
						       body-form replace-fn))
				   (cddr form))))))
	;; (symbol-macrolet) I assume symbol-macrolet gets processed previously
	  (quote form)
	  (return-from (list (car form) (cadr form)
			     (replace-freevars lexenv (caddr form) replace-fn)))
	  (declare form)
	  (t (cons (car form) (mapcar (lambda (arg) (replace-freevars
						lexenv
						arg
						replace-fn)) (cdr form))))))))

(defun list-free-vars (body)
  "Takes a forma and returns its free variables without taking into account the lexical enviroment the body is in.
TODO: change the signature to
(defun list-free-vars (body &optional (lexenv nil)))
"
  (let 
      ((freevars (make-hash-table :test #'equal))
       (freevars-list '()))
    (mapcar (lambda (body-form)
	      (replace-freevars '()
				body-form
				(lambda (freevar)
				  (multiple-value-bind (previous-lexvar found)
				      (gethash freevar freevars)
				    (declare (ignore previous-lexvar))
				    (when (not found)
				      (setf (gethash freevar freevars) freevar))))))
	    body)
    (maphash (lambda (freevar value)
	       (declare (ignore value))
	       (push freevar freevars-list))
	     freevars)
    freevars-list))


(defun list-free-vars-non-external (body)
  "Lists the free vars that were not declared external
through (declare (external ...))"
  (let 
      ((freevars (make-hash-table :test #'equal))
       (freevars-list '())
       (external-vars
	(let ((maybe-declare (car body)))
	  (if (and (equal (string (car maybe-declare)) "DECLARE")
		   (equal (string (caadr maybe-declare)) "EXTERNAL"))
	      (cdadr maybe-declare)
	      '()))))
    (mapcar (lambda (body-form)
	      (replace-freevars '()
				body-form
				(lambda (freevar)
				  (when (not (member freevar external-vars))
				    (multiple-value-bind (previous-lexvar found)
					(gethash freevar freevars)
				      (declare (ignore previous-lexvar))
				      (when (not found)
					(setf (gethash freevar freevars) freevar)))))))
	    body)
    (maphash (lambda (freevar value)
	       (declare (ignore value))
	       (push freevar freevars-list))
	     freevars)
    freevars-list))

(defun prune-externals (code)
  (let ((externals '())
	(non-external-declarations '()))
    (flet ((declaration-p (sentence)
	     (equal (string (car sentence)) "DECLARE"))
	   (declaration (declaration)
	     (string (caadr declaration))))
      (loop for maybe-declaration = (car code)
	 while (declaration-p maybe-declaration)
	 do
	   (progn
	     (setf code (cdr code))
	     (if (equal (declaration maybe-declaration) "EXTERNAL")
	       (setf externals (append (cdadr maybe-declaration) externals))
	       (push maybe-declaration non-external-declarations)
	       ))))
    (values code non-external-declarations externals)))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun xmlize (str)
  "Takes a string and converts it to be XML compatible"
  (setf str (replace-all str "\"" "\\\""))
  (setf str (replace-all str "\n" "\\n"))  ; Think it's wrong. Averiguar cual es carriage return en common lisp en los strings literales
  (replace-all str "</script" "</\"+\"script"))

(defmacro always-with (var &rest body)
  "Takes a var and applies all the forms to it as a first argument "
  (loop for form in body
       collect (cons (car form) (cons var (cdr form)))))

(defun string-alphanumericp (str)
  "Returns if the string contains only alphanumeric characters"
  (reduce
   (lambda (alphanumericp char)
     (and alphanumericp (alphanumericp char)))
   str :initial-value t))

;; Some hunchentoot syntax

#|
(defmacro def-shared-var (name &optional value doc)
  "Defines a hunchentoot shared variable"
  `(progn
     (define-symbol-macro ,name (error (format nil "~A is a shared variable. Use with-shared-var to access it" ',name)))
     (defvar ,(intern (concatenate 'string (symbol-name name) "VAR*")) ,value ,doc)
     (defvar ,(intern (concatenate 'string (symbol-name name) "LOCK*")) (hunchentoot-mp:make-lock ,(concatenate 'string (symbol-name name) "LOCK*")))))
     
(defmacro with-shared-var ((var) &body body)
  "Syntax to use hunchentoot shared variables"
  `(hunchentoot-mp:with-lock (,(intern (concatenate 'string (symbol-name var) "LOCK*")))
     (symbol-macrolet ((,var ,(intern (concatenate 'string (symbol-name var) "VAR*"))))
       ,@body)))

(defmacro with-request-parameters (parameters &body body)
  "Binds hunchentoot current request parameters"
  `(let
       ,(loop for param in parameters
	   collect `(,param (hunchentoot:parameter (string-downcase (string ',param)))))
     ,@body))

(defmacro with-session-values (values &body body)
  "Binds hunchentoot current session values"
  `(let
       ,(loop for value in values
	   collect `(,value (hunchentoot:session-value ',value)))
     ,@body))

|#