(defpackage :lwt.walker
  (:use :cl :arnesi :idioms)
  (:shadow
   #:form
   #:walk-form
   #:make-walk-env
   #:*walk-handlers*
   #:*warn-undefined*
   #:undefined-reference
   #:undefined-variable-reference
   #:undefined-function-reference
   #:return-from-unknown-block
   #:defwalker-handler
   #:implicit-progn-mixin
   #:implicit-progn-with-declare-mixin
   #:binding-form-mixin
   #:declaration-form
   #:constant-form
   #:variable-reference
   #:local-variable-reference
   #:local-lexical-variable-reference
   #:free-variable-reference
   #:application-form
   #:local-application-form
   #:lexical-application-form
   #:free-application-form
   #:lambda-application-form
   #:function-form
   #:lambda-function-form
   #:function-object-form
   #:local-function-object-form
   #:free-function-object-form
   #:lexical-function-object-form
   #:function-argument-form
   #:required-function-argument-form
   #:specialized-function-argument-form
   #:optional-function-argument-form
   #:keyword-function-argument-forms
   #:allow-other-keys-function-argument-form
   #:rest-function-argument-form
   #:block-form
   #:return-from-form
   #:catch-form
   #:throw-form
   #:eval-when-form
   #:if-form
   #:function-binding-form
   #:flet-form
   #:labels-form
   #:variable-binding-form
   #:let-form
   #:let*-form
   #:locally-form
   #:macrolet-form
   #:multiple-value-call-form
   #:multiple-value-prog1-form
   #:progn-form
   #:progv-form
   #:setq-form
   #:symbol-macrolet-form
   #:tagbody-form
   #:go-tag-form
   #:go-form
   #:the-form
   #:unwind-protect-form
   #:extract-argument-names
   #:walk-lambda-list
   #:walk-implicit-progn
   #:arguments
   #:binds
   #:body
   #:cleanup-form
   #:code
   #:consequent
   #:declares
   #:default-value
;; #:else ; iterate
   #:enclosing-tagbody
   #:eval-when-times
   #:first-form
   #:func
   #:keyword-name
   #:name
   #:operator
   #:optimize-spec
   #:other-forms
   #:parent
   #:protected-form
   #:read-only-p
   #:result
   #:source
;; #:specializer ; closer-mop
   #:supplied-p-parameter
   #:tag
   #:target-block
   #:target-progn
   #:then
   #:type-form
   #:value
   #:values-form
   #:var
   #:vars-form))

(in-package :lwt.walker)

;; IMPORTANT: this file contains modifications to the arnesi code walker
;; by Marco Baringer

(defvar *warn-undefined* nil
  "When non-NIL any references to undefined functions or
  variables will signal a warning.")

(defun walk-form (form &optional (parent nil) (env (make-walk-env)))
  "Walk FORM and return a FORM object."
  (funcall (find-walker-handler form) form parent env))

(defun make-walk-env (&optional lexical-env)
  (let ((walk-env '()))
    (when lexical-env
      (dolist (var (lexical-variables lexical-env))
        (extend walk-env :lexical-let var t))
      (dolist (fun (lexical-functions lexical-env))
	(extend walk-env :lexical-flet fun t))
      (dolist (mac (lexical-macros lexical-env))
	(extend walk-env :macrolet (car mac) (cdr mac)))
      (dolist (symmac (lexical-symbol-macros lexical-env))
	(extend walk-env :symbol-macrolet (car symmac) (cdr symmac))))
    (cons walk-env lexical-env)))

(defun copy-walk-env (env)
  "Copy a walk environment"
  (copy-tree env))

(defun register-walk-env (env type name datum &rest other-datum)
  (let ((walk-env (register (car env) type name datum))
	(lexenv (case type
		  (:let (augment-with-variable (cdr env) name))
		  (:macrolet (augment-with-macro (cdr env) name datum))
		  (:flet (augment-with-function (cdr env) name))
		  (:symbol-macrolet (augment-with-symbol-macro (cdr env) name datum))
		  ;;TODO: :declare
		  (t (cdr env)))))
    (cons walk-env lexenv)))

(eval-always
 (defmacro extend-walk-env (env type name datum &rest other-datum)
   `(setf ,env (register-walk-env ,env ,type ,name ,datum ,@other-datum)))

 (defun lookup-walk-env (env type name &key (error-p nil) (default-value nil))
   (lookup (car env) type name :error-p error-p :default-value default-value))

;;;; This takes a Common Lisp form and transforms it into a tree of
;;;; FORM objects.

(defvar *walker-handlers* (make-hash-table :test 'eq))

(define-condition undefined-reference (warning)
  ((enclosing-code :accessor enclosing-code :initform nil)
   (name :accessor name :initarg :name)))

(define-condition undefined-variable-reference (undefined-reference)
  ()
  (:report
   (lambda (c s)
     (if (enclosing-code c)
         (format s "Reference to unknown variable ~S in ~S." (name c) (enclosing-code c))
         (format s "Reference to unknown variable ~S." (name c))))))

(define-condition undefined-function-reference (undefined-reference)
  ()
  (:report
   (lambda (c s)
     (if (enclosing-code c)
         (format s "Reference to unknown function ~S in ~S." (name c) (enclosing-code c))
         (format s "Reference to unknown function ~S." (name c))))))

(defvar +atom-marker+ '+atom-marker+)

(defun find-walker-handler (form)
  "Simple function which tells us what handler should deal
  with FORM. Signals an error if we don't have a handler for
  FORM."
  (if (atom form)
      (gethash '+atom-marker+ *walker-handlers*)
      (aif (gethash (car form) *walker-handlers*)
	   it
	   (case (car form)
	     ((block declare flet function go if labels let let*
		     macrolet progn quote return-from setq symbol-macrolet
		     tagbody unwind-protect catch multiple-value-call
		     multiple-value-prog1 throw load-time-value the
		     eval-when locally progv)
	      (error "Sorry, No walker for the special operator ~S defined." (car form)))
	     (t (gethash 'application *walker-handlers*))))))

(defmacro defwalker-handler (name (form parent lexical-env)
                             &body body)
  `(progn
     (setf (gethash ',name *walker-handlers*)
           (lambda (,form ,parent ,lexical-env)
             (declare (ignorable ,parent ,lexical-env))
             ,@body))
     ',name))

;; We modify the form class to contain the environment
;; in which it was evaluated for further transformations
(defclass form ()
  ((parent :accessor parent :initarg :parent)
   (source :accessor source :initarg :source)
   (environment :accessor environment
		          :initarg :environment
			  :documentation "The environment in which the form was walked.
                                                      Useful for doing later transformations on the tree
                                                      (i.e. using deftransformation)")))

(defmacro macrolet* (definitions &body body)
  (labels
      ((build-macrolet (definitions body)
	 (if (null definitions)
	     `(progn ,@body)
	     `(macrolet ,(list (first definitions)) ,(build-macrolet (rest definitions) body)))))
    (build-macrolet definitions body)))

(defmacro deftransformation (name super-transformations)
  (let ((fname (intern (concatenate 'string (symbol-name name) "-TRANSFORM")))
	(transformation (gensym)))
    `(progn
       (defclass ,name ,super-transformations ())
       (defmacro ,fname (form-type (form parent env) &body body)
	 `(defmethod apply-transformation ((,',transformation ,',name)
					   (,form ,form-type)
					   &optional (parent (slot-value ,',transformation 'parent))
					                    (env (slot-value ,',transformation 'environment)))
	    (flet
		((walk-form (form &optional (parent ,parent) (env ,env))
		   (walk-form form parent env))) ;; We refer to the old walk-form
	      (macrolet*
		  ((wform2wform (wform &optional (parent ,parent) (env ,env))
				 `(apply-transformation ,',',transformation ,wform ,parent ,env))
		   (form2wform (form &optional (parent ,parent) (env ,env))
				 `(unwalk-form
				   (transform-form
				    (walk-form ,form ,parent ,env) ,parent ,env)))
		   (wform2form (form &optional (parent ,parent) (env ,env))
			       `(unwalk-form
				 (transform-form ,form ,parent ,env)))
		   (wforms2wforms (wforms &optional (parent ,parent) (env ,env))
				  (mapcar (lambda (wform) (wform2wform wform))))
		   (wforms2forms (wforms &optional (parent ,parent) (env ,env))
				  (mapcar (lambda (wform) (wform2form wform))))
		   (wbody2wbody (forms parent &optional (env ,env))
				`(progn
				   (setf (slot-value ,parent 'body-env) (copy-walk-env ,env))
				   (mapcar (lambda (form)
					     (wform2wform ,form ,parent ,env))
					   forms)))
		   (wbody2body (forms parent &optional (env ,env))
				`(progn
				   (setf (slot-value ,parent 'body-env) (copy-walk-env ,env))
				   (mapcar (lambda (form)
					     (wform2form ,form ,parent ,env))
					   forms)))
		   ([ (form &optional (parent ,parent) (env ,env))
		      `(walk-form ,form ,parent ,env))
		   ($ (form &optional (parent ,parent) (env ,env))
		      `(wform2form ,form ,parent ,env))
		   ($* (forms &optional (parent ,parent) (env ,env))
		       `(wforms2forms ,forms ,parent ,env))
		   ($b (forms &optional (parent ,parent) (env ,env))
		       `(wbody2body ,forms ,parent ,env)))
		  ,@body))))


(defmacro traverse-and-transform (form-type (form parent env) &body body)
  (with-gensyms (transformation)
		`(defmethod apply-transformation (,transformation
					   (,form ,form-type)
					   &optional (parent (slot-value ,transformation 'parent))
					                    (env (slot-value ,transformation 'environment)))
	    (flet
		((walk-form (form &optional (parent ,parent) (env ,env))
		   (walk-form form parent env))) ;; We refer to the old walk-form
	      (macrolet*
		  ((wform2wform (wform &optional (parent ,parent) (env ,env))
				 `(apply-transformation ,',transformation ,wform ,parent ,env))
		   (form2wform (form &optional (parent ,parent) (env ,env))
				 `(unwalk-form
				   (transform-form
				    (walk-form ,form ,parent ,env) ,parent ,env)))
		   (wform2form (form &optional (parent ,parent) (env ,env))
			       `(unwalk-form
				 (transform-form ,form ,parent ,env)))
		   (wforms2wforms (wforms &optional (parent ,parent) (env ,env))
				  (mapcar (lambda (wform) (wform2wform wform))))
		   (wforms2forms (wforms &optional (parent ,parent) (env ,env))
				  (mapcar (lambda (wform) (wform2form wform))))
		   (wbody2wbody (forms parent &optional (env ,env))
				`(progn
				   (setf (slot-value ,parent 'body-env) (copy-walk-env ,env))
				   (mapcar (lambda (form)
					     (wform2wform ,form ,parent ,env))
					   forms)))
		   (wbody2body (forms parent &optional (env ,env))
				`(progn
				   (setf (slot-value ,parent 'body-env) (copy-walk-env ,env))
				   (mapcar (lambda (form)
					     (wform2form ,form ,parent ,env))
					   forms)))
		   ([ (form &optional (parent ,parent) (env ,env))
		      `(walk-form ,form ,parent ,env))
		   ($ (form &optional (parent ,parent) (env ,env))
		      `(wform2form ,form ,parent ,env))
		   ($* (forms &optional (parent ,parent) (env ,env))
		       `(wforms2forms ,forms ,parent ,env))
		   ($b (forms &optional (parent ,parent) (env ,env))
		       `(wbody2body ,forms ,parent ,env)))
		  ,@body)))))


(defmacro defstandard-transformation (name)
  `(deftransformation ,name (standard-transformation)))

(defclass transformation () ())
(defclass standard-transformation () ())

(defmacro with-form-attributes (attrs form &body body)
  `(with-slots ,attrs ,form ,@body))

;; NOTE: transformations are not extensible with :before :after and :around qualifiers

;; IMPORTANT: to implement transformations and for further uses implement a
;; fold-walked-code function and apply the transformations using that
;; Example: reify-stack
;; With transformations:
(defstandard-transformation reify-stack)


(reify-stack-transform defun-form (form parent env)
		       ;; env is a copy of the form environment
		       ;; parent in the actual form parent
  (let1 env (copy-walk-env env)
	;; walk-lambda-list extends (modifies) the env, that's why we need to copy it
	(walk-form ;; Walks on the original environment
	 `(defun ,name ,(wforms2forms args) ;; This modifies env
	    (progn
	      ,@(loop for arg in
		     (loop for arg in lambda-list-args
			when (one-of ('required-function-argument-form
				                'optional-function-argument-form
						'keyword-function-argument-form) (type-of arg))
			collect (name arg))
		   collect `(def-env-var ',arg ,arg))
	      ,@(wbody2body body form) ;; Compulsory call with the modified env
	      )))))

(reify-stack-transform defun-form (form parent env)
		       ;; env is a copy of the form environment
		       ;; parent in the actual form parent
		       (with-slots (name lambda-list-args body) form
				   (let1 env (copy-walk-env env)
	;; walk-lambda-list extends (modifies) the env, that's why we need to copy it
	([ ;; Walks on the original environment
	 `(defun ,name ,($* args) ;; This modifies env
	    (progn
	      ,@(loop for arg in
		     (loop for arg in lambda-list-args
			when (one-of ('required-function-argument-form
				                'optional-function-argument-form
						'keyword-function-argument-form) (type-of arg))
			collect (name arg))
		   collect `(def-env-var ',arg ,arg))
	      ,@($b body form) ;; Compulsory call with the modified env
	      )))))

(defstandard-transformation cps)

(cps-transform application-form (form parent env)
	    (destructuring-bind (funcall operator &rest args) (unwalk form)
	      ;; Evaluate the arguments from left to right
	      ;; Evaluate the function call in the end
	      ()
 	       ))

;; From cps we've learnt we need to be able to pass additional arguments
;; to transformation functions (in this case the continuation).

;; What about the general transformation? Are them correct?
;; We have to make sure they pass arguments. It's up to the user
;; to modify them to do more complicate things.


(cps-transform progn-form (forms parent env)
	       (cps-transform (first forms) :k
			      (walk
			       `(lambda (,(gensym))
				       ,@(unwalk (cps-transform (walk `(progn ,@(rest forms)) parent env))))
				     parent env)))


;; Now we have to implement transformation cases for when there is no appliable tranformation
;; Note: think of a general recursive transformer so that the user doesn't have to
;; worry about correctly implementing recursion in transformations


;; And transformations are called on arguments and body. Seems
;; too difficult.


;; In order not to transform a piece of code to the infinity, we leave
;; the recursive transform call to the transformers
;; We leave the recursive transform to the transformers to distinguish
;; transformed code from non-transformed code. We don't want to transformed
;; already transformed code forever. Detecting when to transform is a problem
;; when multiple transformation are applied. Solutions:
;; 1) Apply transformations in order only once
;; 2) Mark the nodes with the transformations already applied so that we don't
;;     apply transformations twice. Think of a clean way of marking nodes.
;;     Possible solution: extra-slots concept. These are dynamically bound
;;    slots meant for helping algorithms (i.e. traversals). We have to use
;;    ContextL for this.
;; 3) Mark the nodes in a hash-table where the keys are the visited nodes.

;; Note: automatic recursive transformations are dangerous because
;; we lose control. There are some things we don't want to transform
;; depending on the context probably (the transformation itself). Although
;; that could be fixed with ContextL

(reify-stack-transform setf-form (form parent env)
  (with-slots (place value) form
    (case (type-of place)
	  ('variable-reference
	   (let ((value (unwalk-form value)))
	     (walk-form
	      (once-only (value)
		 `(progn
		    (set-env-var ,(name place) value)
		    ,@(wforms2forms form))))))
	  (t form))))

;; LEARNT: there's no need of implementing a general forms traversers.
;; The traverse (the recursive walking and transforming) is defined in the transformation itself.

;; Now we have to implement the general transformers applier.
;; Take into account that a transformer applied then can produce
;; forms that should be consumed by a previous transformer.
;; The user should be aware of possible loops when appying several transformations


(defgeneric apply-transformation (transformation form parent env))

(defmethod apply-transformation (transformation (form form) parent env)
  form ;; We do nothing
  )

(defmethod apply-transformation (transformation (form if-form) parent env)
  (with-slots (then else) form
	      (setf then (apply-transformation transformation then form env))
	      (setf else (apply-transformation transformation else form env))))

(defmethod apply-transformation (transformation (form setq-form) parent env)
  (with-slots (value) form
	      (setf value (apply-transformation transformation value form env))))

(defmethod apply-transformation (transformation (form let-form) parent env)
  (with-slots (binds body body-env environment) form
    (setf binds (loop for (binding . value) in binds collect (cons binding (apply-transformation transformation bind parent env))))
    ;; The above line should not modify the environment. If that happens, then
    ;; there's a transformation badly designed.
    (setf body (loop for form in body collect (apply-transformation transformation form form body-env)))))

(defmethod apply-transformation (transformation (form flet-form) parent env)
  (with-slots (binds body body-env environment) form
    (setf binds
	  (loop
	     for (name args . body) in binds
	     collect (cons name (apply-transformation (walk-form `(lambda ,args ,@body) form env)))))
    ;; The above line should not modify the environment. If that happens, then
    ;; there's a transformation badly designed.
    (setf body (loop for form in body collect (apply-transformation transformation form form body-env)))))

(defmethod make-load-form ((object form) &optional env)
  (make-load-form-saving-slots object
                               :slot-names (mapcar #'mopp:slot-definition-name
                                                   (mopp:class-slots (class-of object)))
                               :environment (copy-walk-env env)))

(defmethod print-object ((form form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (when (slot-boundp form 'source)
      (let ((*print-readably* nil)
            (*print-level* 0)
            (*print-length* 4))
        (format stream "~S" (source form))))))

(defmacro with-form-object ((variable type &rest initargs)
                            &body body)
  `(let ((,variable (make-instance ',type ,@initargs)))
     ,@body
     ,variable))

(defclass implicit-progn-mixin ()
  ((body-env :accessor body-env :initarg body-env)
   (body :accessor body :initarg :body)))

(defclass implicit-progn-with-declare-mixin (implicit-progn-mixin)
  ((declares :accessor declares :initarg :declares)))

(defclass binding-form-mixin ()
  ((binds :accessor binds :initarg :binds)))

(defmacro multiple-value-setf (places form)
  (loop
       for place in places
       for name = (gensym)
       collect name into bindings
       if (eql 'nil place)
         collect `(declare (ignore ,name)) into ignores
       else
         collect `(setf ,place ,name) into body
       finally (return
                 `(multiple-value-bind ,bindings ,form
                    ,@ignores
                    ,@body))))

(defun split-body (body env &key parent (docstring t) (declare t))
  (let ((documentation nil)
	(newdecls nil)
	(decls nil))
    (flet ((done ()
             (return-from split-body (values body env documentation (nreverse decls)))))
      (loop
         for form = (car body)
         while body
         do (typecase form
              (cons (if (and declare (eql 'cl:declare (first form)))
                        ;; declare form
                        (let ((declarations (rest form)))
                          (dolist* (dec declarations)
                            (multiple-value-setf (env newdecls)
			      (parse-declaration dec env parent))
			    (setf decls (append newdecls decls))))
                        ;; source code, all done
                        (done)))
              (string (if docstring
                          (if documentation
                              ;; already found the docstring, this is source
                              (done)
                              (if (cdr body)
                                  ;; found the doc string
                                  (setf documentation form)
                                  ;; this looks like a doc string, but
                                  ;; it's the only form in body, so
                                  ;; it's actually code.
                                  (done)))
                          ;; no docstring allowed, this is source
                          (done)))
              (t ;; more code, all done
               (done)))
         do (pop body)
         finally (done)))))

(defclass declaration-form (form)
  ())

(defclass optimize-declaration-form (declaration-form)
  ((optimize-spec :accessor optimize-spec :initarg :optimize-spec)))

(defclass variable-declaration-form (declaration-form)
  ((name :accessor name :initarg :name)))

(defclass function-declaration-form (declaration-form)
  ((name :accessor name :initarg :name)))

(defclass dynamic-extent-declaration-form (variable-declaration-form)
  ())

(defclass ignorable-declaration-form-mixin (declaration-form)
  ())

(defclass variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(defclass function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(defclass special-declaration-form (variable-declaration-form)
  ())

(defclass type-declaration-form (variable-declaration-form)
  ((type-form :accessor type-form :initarg :type-form)))

(defclass ftype-declaration-form (function-declaration-form)
  ((type-form :accessor type-form :initarg :type-form)))

(defclass notinline-declaration-form (function-declaration-form)
  ())

(defun parse-declaration (declaration environment parent)
  (let ((declares nil))
    (flet ((funname (form)
	     (if (and (consp form) (eql (car form) 'function))
		 (cadr form)
		 nil)))
      (macrolet ((mkdecl (varname formclass &rest rest)
		   `(make-instance ,formclass
				   :parent parent
				   :source (list type ,varname)
				   :environment (copy-walk-env environment)
				   ,@rest))
		 (extend-env ((var list) newdeclare &rest datum)
		   `(dolist (,var ,list)
		      (when ,newdeclare (push ,newdeclare declares))
                      (extend-walk-env environment :declare ,@datum))))
	(destructuring-bind (type &rest arguments)
	    declaration
	  (case type
	    (dynamic-extent
	     (extend-env (var arguments)
			 (mkdecl var 'dynamic-extent-declaration-form :name var)
			 var `(dynamic-extent)))
	    (ftype
	     (extend-env (function-name (cdr arguments))
			 (make-instance 'ftype-declaration-form
					:parent parent
					:source `(ftype ,(first arguments) function-name)
					:environment (copy-walk-env environment)
					:name function-name
					:type-form (first arguments))
			 function-name `(ftype ,(first arguments))))
	    ((ignore ignorable)
	     (extend-env (var arguments)
			 (aif (funname var)
			      (mkdecl var 'function-ignorable-declaration-form :name it)
			      (mkdecl var 'variable-ignorable-declaration-form :name var))
			 var `(ignorable)))
	    (inline
	      (extend-env (function arguments)
			  (mkdecl function 'function-ignorable-declaration-form :name function)
			  function `(ignorable)))
	    (notinline
	     (extend-env (function arguments)
			 (mkdecl function 'notinline-declaration-form :name function)
			 function `(notinline)))
	    (optimize
	     (extend-env (optimize-spec arguments)
			 (mkdecl optimize-spec 'optimize-declaration-form :optimize-spec optimize-spec)
			 'optimize optimize-spec))
	    (special
	     (extend-env (var arguments)
			 (mkdecl var 'special-declaration-form :name var)
			 var `(special)))
	    (type
	     (extend-env (var (rest arguments))
			 (make-instance 'type-declaration-form
					:parent parent
					:source `(type ,(first arguments) ,var)
					:environment (copy-walk-env environment)
					:name var
					:type-form (first arguments))
			 var `(type ,(first arguments))))
	    (t
	     (extend-env (var arguments)
			 (make-instance 'type-declaration-form
					:parent parent
					:source `(,type ,var)
					:environment (copy-walk-env envirnoment)
					:name var
					:type-form type)
			 var `(type ,type)))))))
    (when (null declares)
      (setq declares (list (make-instance 'declaration-form
					  :parent parent
					  :source declaration
					  :environment (copy-walk-env environment)))))
    (values environment declares)))

(defun walk-implicit-progn (parent forms env &key docstring declare)
  (handler-bind ((undefined-reference (lambda (condition)
                                        (unless (enclosing-code condition)
                                          (setf (enclosing-code condition) `(progn ,@forms))))))
    (multiple-value-bind (body env docstring declarations)
        (split-body forms env :parent parent :docstring docstring :declare declare)
      (values (mapcar (lambda (form)
                        (walk-form form parent env))
                      body)
              docstring
              declarations))))

;;;; Atoms

(defclass constant-form (form)
  ((value :accessor value :initarg :value)))

(defclass variable-reference (form)
  ((name :accessor name :initarg :name)))

(defmethod print-object ((v variable-reference) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~S" (name v))))

(defclass local-variable-reference (variable-reference)
  ())

(defclass local-lexical-variable-reference (local-variable-reference)
  ()
  (:documentation "A reference to a local variable defined in the
  lexical environment outside of the form passed to walk-form."))

(defclass free-variable-reference (variable-reference)
  ())

(defwalker-handler +atom-marker+ (form parent env)
  (declare (special *macroexpand*))
  (cond
    ((not (or (symbolp form) (consp form)))
     (make-instance 'constant-form
		    :value form
                    :parent parent
		    :source form
		    :environment (copy-walk-env env)))
    ((lookup-walk-env env :let form)
     (make-instance 'local-variable-reference
		    :name form
                    :parent parent
		    :source form
		    :environment (copy-walk-env env)))
    ((lookup-walk-env env :lexical-let form)
     (make-instance 'local-lexical-variable-reference
		    :name form
                    :parent parent
		    :source form
		    :environment (copy-walk-env env)))
    ((lookup-walk-env env :symbol-macrolet form)
     (walk-form (lookup-walk-env env :symbol-macrolet form) parent env))
    ((nth-value 1 (macroexpand-1 form))
     ;; a globaly defined symbol-macro
     (walk-form (macroexpand-1 form) parent env))
    (t
     (when (and *warn-undefined*
                (not (boundp form)))
       (warn 'undefined-variable-reference :name form))
     (make-instance 'free-variable-reference
		    :name form
                    :parent parent
		    :source form
		    :environment (copy-walk-env env)))))

;;;; Function Applictation

(defclass application-form (form)
  ((operator :accessor operator :initarg :operator)
   (arguments :accessor arguments :initarg :arguments)))

(defclass local-application-form (application-form)
  ((code :accessor code :initarg :code)))

(defclass lexical-application-form (application-form)
  ())

(defclass free-application-form (application-form)
  ())

(defclass lambda-application-form (application-form)
  ())

(defwalker-handler application (form parent env)
  (block nil
    (destructuring-bind (op &rest args)
        form
      (when (and (consp op)
                 (eq 'cl:lambda (car op)))
        (return
          (with-form-object (application lambda-application-form
					 :parent parent
					 :source form
					 :environment (copy-walk-env env))
            (setf (operator application) (walk-form op application env)
                  (arguments application) (mapcar (lambda (form)
                                                    (walk-form form application env))
                                                  args)))))
      (when (lookup-walk-env env :macrolet op)
        (return (walk-form (funcall (lookup-walk-env env :macrolet op) form (cdr env)) parent env)))
      (when (and (symbolp op) (macro-function op))
	(multiple-value-bind (expansion expanded)
	    (macroexpand-1 form (cdr env))
	  (when expanded
	    (return (walk-form expansion parent env)))))
      (let ((app (if (lookup-walk-env env :flet op)
                     (make-instance 'local-application-form
				    :code (lookup-walk-env env :flet op)
				    :environment (copy-walk-env env))
                     (if (lookup-walk-env env :lexical-flet op)
			 (make-instance 'lexical-application-form
					:environment (copy-walk-env env))
                         (progn
                           (when (and *warn-undefined*
                                      (symbolp op)
                                      (not (fboundp op)))
                             (warn 'undefined-function-reference :name op))
                           (make-instance 'free-application-form
					  :environment (copy-walk-env env)))))))
        (setf (operator app) op
              (parent app) parent
              (source app) form
              (arguments app) (mapcar (lambda (form)
                                        (walk-form form app env))
                                      args))
        app))))

;;;; Functions

(defclass function-form (form)
  ())

(defclass lambda-function-form (function-form implicit-progn-with-declare-mixin)
  ((arguments :accessor arguments :initarg :arguments)))

(defclass function-object-form (form)
  ((name :accessor name :initarg :name)))

(defclass local-function-object-form (function-object-form)
  ())

(defclass free-function-object-form (function-object-form)
  ())

(defclass lexical-function-object-form (function-object-form)
  ())

(defwalker-handler function (form parent env)
  (if (and (listp (second form))
           (eql 'cl:lambda (first (second form))))
      ;; (function (lambda ...))
      (walk-lambda (second form) parent env)
      ;; (function foo)
      (make-instance (if (lookup-walk-env env :flet (second form))
                         'local-function-object-form
                         (if (lookup-walk-env env :lexical-flet (second form))
			     'lexical-function-object-form
			     'free-function-object-form))
                     :name (second form)
                     :parent parent
		     :source form
		     :environment (copy-walk-env env))))

(defun walk-lambda (form parent env)
  (with-form-object (func lambda-function-form
                          :parent parent
                          :source form
			  :environment (copy-walk-env env))
    ;; 1) parse the argument list creating a list of FUNCTION-ARGUMENT-FORM objects
    (multiple-value-setf ((arguments func) env)
      (walk-lambda-list (second form) func env))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    ;; 2) parse the body
    (multiple-value-setf ((body func) nil (declares func))
      (walk-implicit-progn func (cddr form) env :declare t))
    ;; all done
    func))

(defmethod apply-transformation (transformation (form lambda-function-form) parent env)
  (let1 env (copy-walk-env env)
	(with-slots (arguments body body-env) form
		    (setf arguments
			  (mapcar
			   (lambda (arg)
			     (apply-transformation transformation transformation arg parent env))))
		    ;; The above could have changed the enviroment. For example,
		    ;; ranamed a variable.
		    (setf body (mapcar (lambda (form) (apply-transformation transformation form parent body-env)))))))

(defmethod apply-transformation (transformation (form lambda-function-form) parent env)
  (with-slots (arguments body) form
	      ([
	       `(lambda ,($* arguments)
		  ,@($b body)))))

;; So now we have that apply-transformation traversals are transformations too

;; Transformation helpers??
;; Should be a deftransformation flet so that we can get rid of parent and env
(defun rename-lambda-argument (var new-name &optional parent env)

  )

(defmethod rename-binding ((form let-form) binding new-name)
  (with-slots (binds body body-env) form
	      (loop for (var . value) in binds
		    if (equal var binding) do
		    (setf var binding)
		    (return))
	      (setf body-env (make-env-from-binds binds))
	      (setf body (mapcar (lambda (form)
				                     (walk-form (unwalk-form form) parent body-env))))))

(defun rename-let)

;; Note that a renaming implies a complete rewalking of the body
;; ((walk (unwalk body) parent renaming-env)


(defun walk-lambda-list (lambda-list parent env &key allow-specializers macro-p)
  (flet ((extend-env (argument)
           (unless (typep argument 'allow-other-keys-function-argument-form)
             (extend-walk-env env :let (name argument) argument))))
    (let ((state :required)
          (arguments '()))
      (dolist (argument lambda-list)
        (if (member argument '(&optional &key &rest))
            (setf state argument)
            (progn
              (push (case state
                      (:required
                       (if allow-specializers
                           (walk-specialized-argument-form argument parent env)
                           (walk-required-argument argument parent env)))
                      (&optional (walk-optional-argument argument parent env))
                      (&key
                       (if (eql '&allow-other-keys argument)
                           (make-instance 'allow-other-keys-function-argument-form
                                          :parent parent
					  :source argument
					  :environment (copy-walk-env env))
                           (walk-keyword-argument argument parent env)))
                      (&rest (walk-rest-argument argument parent env)))
                    arguments)
              (extend-env (car arguments)))))
      (values (nreverse arguments) env))))

(defclass function-argument-form (form)
  ((name :accessor name :initarg :name)))

(defmethod print-object ((argument function-argument-form) stream)
  (print-unreadable-object (argument stream :type t :identity t)
    (if (slot-boundp argument 'name)
        (format stream "~S" (name argument))
        (write-string "#<unbound name>" stream))))

(defclass required-function-argument-form (function-argument-form)
  ())

(defgeneric required-function-argument-form-p (object)
  (:method ((object t)) nil)
  (:method ((object required-function-argument-form)) t))

(defun walk-required-argument (form parent env)
  (declare (ignore env))
  (make-instance 'required-function-argument-form
                 :name form
                 :parent parent
		 :source form
		 :environment (copy-walk-env env)))

(defclass specialized-function-argument-form (required-function-argument-form)
  ((specializer :accessor specializer :initarg :specializer)))

(defun walk-specialized-argument-form (form parent env)
  (declare (ignore env))
  (make-instance 'specialized-function-argument-form
                 :name (if (listp form)
                           (first form)
                           form)
                 :specializer (if (listp form)
                                  (second form)
                                  'T)
                 :parent parent
                 :source form
		 :environment (copy-walk-env env)))

(defclass optional-function-argument-form (function-argument-form)
  ((default-value :accessor default-value :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun walk-optional-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (with-form-object (arg optional-function-argument-form
                           :parent parent
                           :source form
			   :environment (copy-walk-env env)
                           :name name
                           :supplied-p-parameter supplied-p-parameter)
      (setf (default-value arg) (walk-form default-value arg env)))))

(defclass keyword-function-argument-form (function-argument-form)
  ((keyword-name :accessor keyword-name :initarg :keyword-name)
   (default-value :accessor default-value :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defmethod effective-keyword-name ((k keyword-function-argument-form))
  (or (keyword-name k)
      (intern (symbol-name (name k)) :keyword)))

(defun walk-keyword-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (let ((name (if (consp name)
                    (second name)
                    name))
          (keyword (if (consp name)
                       (first name)
                       nil)))
      (with-form-object (arg keyword-function-argument-form
                             :parent parent
                             :source form
			     :environment (copy-walk-env env)
                             :name name
                             :keyword-name keyword
                             :supplied-p-parameter supplied-p-parameter)
        (setf (default-value arg) (walk-form default-value arg env))))))

(defclass allow-other-keys-function-argument-form (function-argument-form)
  ())

(defclass rest-function-argument-form (function-argument-form)
  ())

(defun walk-rest-argument (form parent env)
  (declare (ignore env))
  (make-instance 'rest-function-argument-form
		 :name form
                 :parent parent
		 :source form
		 :environment (copy-walk-env env)))

;;;; BLOCK/RETURN-FROM

(defclass block-form (form implicit-progn-mixin)
  ((name :accessor name :initarg :name)))

(defclass return-from-form (form)
  ((target-block :accessor target-block :initarg :target-block)
   (result :accessor result :initarg :result)))

(defwalker-handler block (form parent env)
  (destructuring-bind (block-name &rest body)
      (cdr form)
    (with-form-object (block block-form
                       :parent parent
		       :source form
                       :name block-name
		       :environment (copy-walk-env env))
      (setf (slot-value form 'body-env) (copy-walk-env env))
      (setf (body block) (walk-implicit-progn block
                                             body
                                             (register-walk-env env :block block-name block))))))

(define-condition return-from-unknown-block (serious-condition)
  ((block-name :accessor block-name :initarg :block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to return from block named ~S." (block-name condition)))))

(defwalker-handler return-from (form parent env)
  (destructuring-bind (block-name &optional (value '(values)))
      (cdr form)
    (if (lookup-walk-env env :block block-name)
        (with-form-object (return-from return-from-form
			    :parent parent
			    :source form
			    :environment (copy-walk-env env)
			    :target-block (lookup-walk-env env :block block-name))
          (setf (result return-from) (walk-form value return-from env)))
        (restart-case
            (error 'return-from-unknown-block :block-name block-name)
          (add-block ()
            :report "Add this block and continue."
            (walk-form form parent (register-walk-env env :block block-name :unknown-block)))))))

;;;; CATCH/THROW

(defclass catch-form (form implicit-progn-mixin)
  ((tag :accessor tag :initarg :tag)))

(defclass throw-form (form)
  ((tag :accessor tag :initarg :tag)
   (value :accessor value :initarg :value)))

(defwalker-handler catch (form parent env)
  (destructuring-bind (tag &body body)
      (cdr form)
    (with-form-object (catch catch-form
			:parent parent
			:source form
			:environment (copy-walk-env env))
      (setf (slot-value form 'body-env) (copy-walk-env env))
      (setf (tag catch) (walk-form tag catch env)
            (body catch) (walk-implicit-progn catch body env)))))

(defwalker-handler throw (form parent env)
  (destructuring-bind (tag &optional (result '(values)))
      (cdr form)
    (with-form-object (throw throw-form
			:parent parent
			:source form
			:environment (copy-walk-env env))
      (setf (tag throw) (walk-form tag throw env)
            (value throw) (walk-form result throw env)))))

;;;; EVAL-WHEN

(defclass eval-when-form (form implicit-progn-mixin)
  ((eval-when-times :accessor eval-when-times :initarg :eval-when-times)))

(defwalker-handler eval-when (form parent env)
  (destructuring-bind (times &body body)
      (cdr form)
    (with-form-object (eval-when eval-when-form
			:parent parent
			:source form
			:environment (copy-walk-env env))
      (setf (slot-value form 'body-env) (copy-walk-env env))
      (setf (eval-when-times eval-when) times
            (body eval-when) (walk-implicit-progn eval-when body env)))))

;;;; IF

(defclass if-form (form)
  ((consequent :accessor consequent :initarg :consequent)
   (then :accessor then :initarg :then)
   (else :accessor else :initarg :else)))

(defwalker-handler if (form parent env)
  (with-form-object (if if-form
			:parent parent
			:source form
			:environment (copy-walk-env env))
    (setf (consequent if) (walk-form (second form) if env)
          (then if) (walk-form (third form) if env)
          (else if) (walk-form (fourth form) if env))))

;;;; FLET/LABELS

(defclass function-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass flet-form (function-binding-form)
  ())

(defclass labels-form (function-binding-form)
  ())

(defwalker-handler flet (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (flet flet-form
			:parent parent
			:source form
			:environment (copy-walk-env env))
      ;;;; build up the objects for the bindings in the original env
      (loop
         for (name args . body) in binds
         collect (cons name (walk-form `(lambda ,args ,@body) flet env)) into bindings
         finally (setf (binds flet) bindings))
      ;;;; walk the body in the new env
      (setf (slot-value form 'body-env) (copy-walk-env env))
      (multiple-value-setf ((body flet) nil (declares flet))
	(walk-implicit-progn flet
			    body
			    (loop
			       with env = env
			       for (name . lambda) in (binds flet)
			       do (extend-walk-env env :flet name lambda)
			       finally (return env))
			    :declare t)))))

(defwalker-handler labels (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (labels labels-form
			:parent parent
			:source form
			:environment (copy-walk-env env)
			:binds '())
      ;; we need to walk over the bindings twice. the first pass
      ;; creates some 'empty' lambda objects in the environment so
      ;; that local-application-form and local-function-object-form
      ;; have something to point to. the second pass then walks the
      ;; actual bodies of the form filling in the previously created
      ;; objects.
      (loop
         for (name arguments . body) in binds
         for lambda = (make-instance 'lambda-function-form
                                     :parent labels
                                     :source (list* name arguments body)
				     :environment (copy-walk-env env))
         do (push (cons name lambda) (binds labels))
         do (extend-walk-env env :flet name lambda))
      (setf (binds labels) (nreverse (binds labels)))
      (loop
         for form in binds
         for (arguments . body) = (cdr form)
         for binding in (binds labels)
         for lambda = (cdr binding)
         for tmp-lambda = (walk-lambda `(lambda ,arguments ,@body) labels env)
         do (setf (body lambda) (body tmp-lambda)
                  (arguments lambda) (arguments tmp-lambda)
		  (declares lambda) (declares tmp-lambda)))
      (setf (slot-value form 'body-env) (copy-walk-env env))
      (multiple-value-setf ((body labels) nil (declares labels))
	(walk-implicit-progn labels body env :declare t)))))

;;;; LET/LET*

(defclass variable-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass let-form (variable-binding-form)
  ())

(defwalker-handler let (form parent env)
  (with-form-object (let let-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env))
    (setf (binds let) (mapcar (lambda (binding)
                                   (destructuring-bind (var &optional initial-value)
                                       (ensure-list binding)
                                     (cons var (walk-form initial-value let env))))
                                 (second form)))
    (multiple-value-bind (b e d declarations)
        (split-body (cddr form) env :parent let :declare t)
      (declare (ignore b e d))
      (dolist* ((var . value) (binds let))
        (declare (ignore value))
        (if (not (find-if (lambda (declaration)
                            (and (typep declaration 'special-declaration-form)
                                 (eq var (name declaration)))) declarations))
            (extend-walk-env env :let var :dummy)))
      (setf (slot-value form 'body-env) (copy-walk-env env))
      (multiple-value-setf ((body let) nil (declares let))
	(walk-implicit-progn let (cddr form) env :declare t)))))

(defclass let*-form (variable-binding-form)
  ())

(defwalker-handler let* (form parent env)
  (with-form-object (let* let*-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env)
		      :binds '())
    (dolist* ((var &optional initial-value) (mapcar #'ensure-list (second form)))
      (push (cons var (walk-form initial-value let* env)) (binds let*))
      (extend-walk-env env :let var :dummy))
    (setf (binds let*) (nreverse (binds let*)))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (multiple-value-setf ((body let*) nil (declares let*))
      (walk-implicit-progn let* (cddr form) env :declare t))))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((value :accessor value)
   (read-only-p :accessor read-only-p)))

(defwalker-handler load-time-value (form parent env)
  (with-form-object (load-time-value load-time-value-form
                                     :parent parent
				     :source form
				     :environment (copy-walk-env env))
    (setf (value load-time-value) (walk-form (second form) load-time-value env)
          (read-only-p load-time-value) (third form))))

;;;; LOCALLY

(defclass locally-form (form implicit-progn-with-declare-mixin)
  ())

(defwalker-handler locally (form parent env)
  (with-form-object (locally locally-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (multiple-value-setf ((body locally) nil (declares locally))
      (walk-implicit-progn locally (cdr form) env :declare t))))

;;;; MACROLET

(defclass macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler macrolet (form parent env)
  (with-form-object (macrolet macrolet-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env)
		      :binds '())
    (dolist* ((name args &body body) (second form))
      (let ((handler (parse-macro-definition name args body (cdr env))))
        (extend-walk-env env :macrolet name handler)
        (push (cons name handler) (binds macrolet))))
    (setf (binds macrolet) (nreverse (binds macrolet)))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (multiple-value-setf ((body macrolet) nil (declares macrolet))
      (walk-implicit-progn macrolet (cddr form) env :declare t))))

;;;; MULTIPLE-VALUE-CALL

(defclass multiple-value-call-form (form)
  ((func :accessor func :initarg :func)
   (arguments :accessor arguments :initarg :arguments)))

(defwalker-handler multiple-value-call (form parent env)
  (with-form-object (m-v-c multiple-value-call-form
			   :parent parent
			   :source form
			   :environment (copy-walk-env env))
    (setf (func m-v-c) (walk-form (second form) m-v-c env)
          (arguments m-v-c) (mapcar (lambda (f) (walk-form f m-v-c env))
                                    (cddr form)))))

;;;; MULTIPLE-VALUE-PROG1

(defclass multiple-value-prog1-form (form)
  ((first-form :accessor first-form :initarg :first-form)
   (other-forms :accessor other-forms :initarg :other-forms)))

(defwalker-handler multiple-value-prog1 (form parent env)
  (with-form-object (m-v-p1 multiple-value-prog1-form
			    :parent parent
			    :source form
			    :environment (copy-walk-env env))
    (setf (first-form m-v-p1) (walk-form (second form) m-v-p1 env)
          (other-forms m-v-p1) (mapcar (lambda (f) (walk-form f m-v-p1 env))
                                       (cddr form)))))

;;;; PROGN

(defclass progn-form (form implicit-progn-mixin)
  ())

(defwalker-handler progn (form parent env)
  (with-form-object (progn progn-form
			   :parent parent
			   :source form
			   :environment (copy-walk-env env))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (setf (body progn) (walk-implicit-progn progn (cdr form) env))))

;;;; PROGV

(defclass progv-form (form implicit-progn-mixin)
  ((vars-form :accessor vars-form :initarg :vars-form)
   (values-form :accessor values-form :initarg :values-form)))

(defwalker-handler progv (form parent env)
  (with-form-object (progv progv-form
			:parent parent
			:source form
			:environment (copy-walk-env env))
    (setf (vars-form progv) (walk-form (cadr form) progv env))
    (setf (values-form progv) (walk-form (caddr form) progv env))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (setf (body progv) (walk-implicit-progn progv (cdddr form) env))
    progv))

;;;; QUOTE

(defwalker-handler quote (form parent env)
  (make-instance 'constant-form
		 :parent parent
		 :source form
		 :environment (copy-walk-env env)
		 :value (second form)))

;;;; SETQ

(defclass setq-form (form)
  ((var   :accessor var   :initarg :var)
   (value :accessor value :initarg :value)))

(defwalker-handler setq (form parent env)
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       for (name value) on (cdr form) by #'cddr
       if (lookup-walk-env env :symbol-macrolet name)
         do (push `(setf ,(lookup-walk-env env :symbol-macrolet name) ,value) effective-code)
       else
         do (push `(setq ,name ,value) effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (destructuring-bind (type var value)
            (first effective-code)
          (ecase type
            (setq (with-form-object (setq setq-form
					  :parent parent
					  :source form
					  :environment (copy-walk-env env)
                                          :var var)
                    (setf (value setq) (walk-form value setq env))))
            (setf (walk-form (first effective-code) parent env))))
        ;; multiple forms
        (with-form-object (progn progn-form
				 :parent parent
				 :source form
				 :environment (copy-walk-env env))
	  (setf (slot-value form 'body-env) (copy-walk-env env))
          (setf (body progn) (walk-implicit-progn progn effective-code env))))))

;;;; SYMBOL-MACROLET

(defclass symbol-macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler symbol-macrolet (form parent env)
  (with-form-object (symbol-macrolet symbol-macrolet-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env)
		      :binds '())
    (dolist* ((symbol expansion) (second form))
      (extend-walk-env env :symbol-macrolet symbol expansion)
      (push (cons symbol expansion) (binds symbol-macrolet)))
    (setf (binds symbol-macrolet) (nreverse (binds symbol-macrolet)))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (multiple-value-setf ((body symbol-macrolet) nil (declares symbol-macrolet))
      (walk-implicit-progn symbol-macrolet (cddr form) env :declare t))))

;;;; TAGBODY/GO

(defclass tagbody-form (form implicit-progn-mixin)
  ())

(defclass go-tag-form (form)
  ((name :accessor name :initarg :name)))

(defgeneric go-tag-form-p (object)
  (:method ((object go-tag-form)) t)
  (:method ((object t))           nil))

(defwalker-handler tagbody (form parent env)
  (with-form-object (tagbody tagbody-form
		     :parent parent
		     :source form
		     :environment (copy-walk-env env)
		     :body (cdr form))
    (extend-walk-env env :tagbody 'enclosing-tagbody tagbody)
    (flet ((go-tag-p (form)
             (or (symbolp form) (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body tagbody) (copy-list (body tagbody)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (extend-walk-env env :tag (car part) (cdr part)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (setf (car part) (make-instance 'go-tag-form
					      :parent tagbody
                                              :source (car part)
					      :environment (copy-walk-env env)
                                              :name (car part)))
         else
           do (setf (car part) (walk-form (car part) tagbody env))))))

(defclass go-form (form)
  ((target-progn :accessor target-progn :initarg :target-progn)
   (name :accessor name :initarg :name)
   (enclosing-tagbody :accessor enclosing-tagbody :initarg :enclosing-tagbody)))

(defwalker-handler go (form parent env)
  (make-instance 'go-form
                 :parent parent
                 :source form
		 :environment (copy-walk-env env)
                 :name (second form)
                 :target-progn (lookup-walk-env env :tag (second form))
                 :enclosing-tagbody (lookup-walk-env env :tagbody 'enclosing-tagbody)))

;;;; THE

(defclass the-form (form)
  ((type-form :accessor type-form :initarg :type-form)
   (value :accessor value :initarg :value)))

(defwalker-handler the (form parent env)
  (with-form-object (the the-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env)
		      :type-form (second form))
    (setf (value the) (walk-form (third form) the env))))

;;;; UNWIND-PROTECT

(defclass unwind-protect-form (form)
  ((protected-form :accessor protected-form :initarg :protected-form)
   (cleanup-form :accessor cleanup-form :initarg :cleanup-form)))

(defwalker-handler unwind-protect (form parent env)
  (with-form-object (unwind-protect unwind-protect-form
		      :parent parent
		      :source form
		      :environment (copy-walk-env env))
    (setf (slot-value form 'body-env) (copy-walk-env env))
    (setf (protected-form unwind-protect) (walk-form (second form) unwind-protect env)
          (cleanup-form unwind-protect) (walk-implicit-progn unwind-protect (cddr form) env))))

;;;; ** Implementation specific walkers

;;;; These are for forms which certain compilers treat specially but
;;;; aren't macros or special-operators.

#+lispworks
(defwalker-handler compiler::internal-the (form parent env)
  (walk-form (third form) parent env))

;; Copyright (c) 2005-2006, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


