(in-package :gstutils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defalias (fname &rest aliases)
    (let ((setf-args '()))
      `(progn
	 (setf
	  ,@(loop for alias in aliases
	       do (setf setf-args (nconc setf-args (list `(symbol-function ',alias) `(function ,fname))))
	       finally (return setf-args)))
	 ',aliases))))


;; Free variables manipulation
;; See dataflow package for examples

(defun list-lambda-list-vars (lambda-list)
  "Lists the vars or a lambda function arguments"
  
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
				 morep more-context more-count)
      (parse-ordinary-lambda-list lambda-list)
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


(defun dynamic-variable-symbol-p (symbol)
  "Returns if the symbol refers to a dynamic variable.
   Comment: that's not really true, we are just cheking for syntactic issues (look for the asterisks). We have no way to decide statically (at macroexpansion time)"
  (flet
      ((string-starts-with (character string)
	 (equalp character
		 (aref string 0)))
       (string-ends-with (character string)
	 (equalp character
		 (aref string (- (length string) 1)))))
  (and (symbolp symbol)
       (string-starts-with #\* (symbol-name symbol))
       (string-ends-with #\* (symbol-name symbol)))))


(define-condition replacement-not-found-error (serious-condition)
  ())

(defvar *replacements* (make-hash-table :test #'equalp)
  "Replacement functions")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-replacement ((pattern &rest pattern-args) &rest body)
    (with-unique-names (tag form replace-fn lexenv)
      `(setf (gethash ',pattern *replacements*)
	     (lambda (,form ,replace-fn ,lexenv)
	       (declare (ignorable ,lexenv ,replace-fn))
	       (flet ((replace-fvars (form lexenv)
			(replace-freevars form ,replace-fn lexenv)))
		 (destructuring-bind (,tag ,@pattern-args) ,form
		   (declare (ignore ,tag))
		   (symbol-macrolet ((lexenv ,lexenv))
		     ,@body))))))))

(defun get-replacement (symbol)
  (multiple-value-bind (replacement foundp)
      (gethash symbol *replacements*)
    (if (not foundp)
	(error 'replacement-not-found-error "Replacement for ~A not found. Define one with def-replacement" symbol)
	replacement)))

(defun replace-freevars (form replace-fn &optional (lexenv nil))
  "Takes a form and a replace function and applies it to the form's free variables
   and replace them by the function's result. It takes a lexical environment; a list of local variable names.
   Returns a new form with the replacements applied."
  (flet
      ((process-list-form (form)
	 (let ((replacement (get-replacement (car form))))
	   (funcall replacement form replace-fn lexenv))))
  (cond
    ;; Constant or keyword
    ((or (keywordp form)
	 (constantp form))
     form)
    ;; A lexical variable
    ((and (atom form) (member form lexenv))
     form)
    ;; A dynamic variable
    ((and (atom form) (dynamic-variable-symbol-p form))
     form)
    ;; A free variable
    ((atom form)
     (funcall replace-fn form))
    (t
     (handler-case (process-list-form form)
       (replacement-not-found-error ()
	 (destructuring-bind
	       (form-tag &rest form-body) form
	 (cons form-tag (mapcar (lambda (body-form)
				  (replace-freevars body-form
						    replace-fn
						    lexenv))
				  form-body)))))))))

"Lambda replacement"
(def-replacement (lambda args &body body)
    (let ((new-lexenv (append (list-lambda-list-vars args)
			      lexenv)))
      `(lambda ,args
	 ,@(mapcar (lambda (body-form)
		     (replace-fvars body-form
				    new-lexenv))
		   body))))

"let replacement"
(def-replacement (let bindings &rest body)
    (let* ((bindings (loop for binding in bindings
		      collect (destructuring-bind (binding-name binding-expr) binding
				(list binding-name
				      (replace-fvars binding-expr
						     lexenv)))))
	   (new-lexenv (append (mapcar #'car bindings) lexenv)))
    `(let ,bindings
       ,@(mapcar (lambda (body-form)
		  (replace-fvars body-form
				 new-lexenv))
		     body))))

"let* replacement"
(def-replacement (let* bindings &rest body)
  (let* ((new-lexenv lexenv)
	 (bindings (loop for binding in bindings
		      collect (destructuring-bind (binding-name binding-expr) binding
				(aprog1
				    (list binding-name
					  (replace-fvars binding-expr
							 new-lexenv))
				  (push (car it) new-lexenv)
				  it)))))
    `(let* ,bindings
       ,@(mapcar (lambda (body-form)
		  (replace-fvars body-form
				 new-lexenv))
		body))))


"flet replacement"
(def-replacement (flet bindings &rest body)
    (let* ((new-lexenv '())
	   (bindings (loop for binding in bindings
			collect
			  (destructuring-bind
				(f-name f-args &rest f-body) binding
			    (let* ((local-lexenv (append lexenv
							 (list-lambda-list-vars f-args)))
				   (new-binding `(,f-name ,f-args
							  ,@(mapcar (lambda (body-form)
								    (replace-fvars body-form
										   local-lexenv))
								  f-body))))
			      (push f-name new-lexenv)
			      new-binding)))))
      (let ((lexenv (append new-lexenv lexenv)))
	`(flet ,bindings
	   ,@(mapcar (lambda (body-form)
		       (replace-fvars body-form
				      new-lexenv))
		     body)))))

"labels replacement"
(def-replacement (labels bindings &rest body)
    (let* ((new-lexenv '())
	   (bindings (loop for binding in bindings
			collect
			  (destructuring-bind
				(f-name f-args &rest f-body) binding
			    (let* ((local-lexenv (append new-lexenv
							 (list-lambda-list-vars f-args)
							 (list f-name)))
				   (new-binding `(,f-name ,f-args
							  ,@(mapcar (lambda (body-form)
								    (replace-fvars body-form
										   local-lexenv))
								f-body))))
			      (push f-name new-lexenv)
			      new-binding)))))
      (let ((new-lexenv (append new-lexenv lexenv)))
	`(labels ,bindings
	   ,@(mapcar (lambda (body-form)
		       (replace-fvars body-form
				      new-lexenv))
		  body)))))

;; quote
(def-replacement (quote form)
    `(quote ,form))

;; return-from
(def-replacement (return-from name &optional expr)
    `(return-from ,name
       ,(replace-fvars expr lexenv)))

;; declare
(def-replacement (declare form)
  `(declare ,form))

(defun list-free-vars (body &optional (lexenv nil))
  "Takes a form and returns its free variables without
   taking into account the lexical enviroment the body is in"
  (let 
      ((freevars (make-hash-table :test #'equalp))
       (freevars-list '()))
    (mapcar (lambda (body-form)
	      (replace-freevars body-form
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
    (remove-if (lambda (freevar)
		 (member freevar lexenv))
	       freevars-list)))

(defun list-free-vars-non-external (body &optional (lexenv nil))
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
	      (replace-freevars body-form
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
    (remove-if (lambda (freevar)
		 (member freevar lexenv))
	       freevars-list)))

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
	       (push maybe-declaration non-external-declarations)))))
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