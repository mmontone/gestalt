(defpackage :lwt.stream
  (:use :common-lisp :arnesi)
  (:export
   #:*standard-stream*
   #:stream-value
   #:start-streaming
   #:consume-stream
   #:produce-stream
   #:stream*
   #:broadcast-stream*
   #:quit-streaming
   #:quit-process))

(in-package :lwt.stream)

(defclass stream* ()
  ((value :accessor stream-value :initform nil)
   (consumers :accessor stream-consumers :initform '())
   (producers :accessor stream-producers :initform '())))

(defclass broadcast-stream* (stream*)
  ())

(define-condition quit-streaming-condition () ())
(define-condition quit-process-condition () ())

(defun quit-streaming ()
  (error 'quit-streaming-condition))

(defun quit-process ()
  (error 'quit-process-condition))

(defvar *standard-stream* (make-instance 'broadcast-stream*))


(defmethod start-streaming ((stream broadcast-stream*))
  (handler-case
      (with-slots (value consumers producers) stream
	(loop while t do
	     (loop for producer in producers do
		  (progn
		    (setf value (funcall producer))
		    (loop for consumer in consumers
		       do (handler-case
			      (funcall consumer value)
			    (quit-process-condition ()
			      (setf consumers (remove consumer consumers)))))))))
    (quit-streaming-condition () (values))))

(defmethod consume-stream ((stream stream*) func)
  (with-slots (consumers) stream
    (push func consumers)))

(defmethod produce-stream ((stream stream*) func)
  (with-slots (producers) stream
    (push func producers)))

;; Examples:

(consume-stream *standard-stream*
		(lambda (value)
		  (if (equalp value 5)
		      (quit-process)
		      (format t "A Consuming value: ~A~%" value))))

(consume-stream *standard-stream*
		(lambda (value)
		  (block consumer
		    (if (equalp value 5)
			(return-from consumer)
			(format t "B Consuming value: ~A~%" value)))))

(defvar *value* 0)
(produce-stream *standard-stream*
		(lambda ()
		  (if (equalp *value* 11)
		      (quit-streaming)
		      (let
			  ((val *value*))
			(incf *value*)
			(format t "Producing value: ~A~%" val)
			val))))

(defun start-example ()
  (setf *standard-stream* (make-instance 'broadcast-stream*))
  (setf *value* 0)
  (start-streaming *standard-stream*))

;; Now use this code to prototype client-server interaction for lwt

(handler-case
    (asdf)
  (undefined-function (c)
    (format t "Name ~A~%" (cell-error-name c))
    (swank:inspect-in-emacs c)))

;; Idea:
;; We have to wrap the code so that the undefined-function* exception holds the
;; current continuation.
;; We have to change the cps transformers and be aware of
;; performance issues

(defvar *client-stream* (make-instance 'broadcast-stream*) "Data sent to the client")
(defvar *server-stream* (make-instance 'broadcast-stream*) "Data sent to the server")

(defvar *k* #'identity "The current continuation")

(defun sum-action ()
  (funcall
      (lambda (k1)
	(funcall k1 (rem-bind (prompt "Enter a number"))))
    (lambda (n1)
      (funcall
	  (lambda (k2)
	    (funcall k2 (rem-bind (prompt "Enter another number"))))
	  (lambda (n2)
	    ;; Note that I have n1 in the lex env ;)
	    ;; Cause n1 is "from the past"
	  (in-server (+ n1 n2)))))))

;; The above code will be different to generate cause we need to pass
;; the current continuation (correct k? to rem-bind)
;; In the following code, we keep the current continuation in a dyn var
(defun sum-action ()
  (let
      ((*k* (lambda (n1)
	      (let
		  ((*k*
		    (lambda (n2)
		      ;; Note that I have n1 in the lex env ;)
		      ;; Cause n1 is "from the past"
		      (in-server (+ n1 n2)))))
		(funcall *k* (rem-bind (prompt "Enter another number")))))))
      (funcall *k* (rem-bind (prompt "Enter a number")))))

;; That simplifies code generation, although now you have to read the code backwards

(define-condition undefined-function* ()
  ((form :accessor uf-form :initarg :form :initform (error "Supply the form"))
   (k :accessor uf-cont :initarg :k :initform (error "Supply the continuation"))))

(defmacro rem-bind (form)
  `(handler-case
       ,form
     (undefined-function ()
       (error 'undefined-function* :form ',form :k *k*))))

(defmacro in-server (form)
  `(error 'undefined-function* :form ',form :k *k*))

(defun prompt (message &rest args)
  (format t message args)
  (read-line))

(defun display (message &rest args)
  (format t message args))

;; Option 2: resolve every call at compile-time


;; dynvar test
(defvar *my-dyn* nil)

(let ((*my-dyn* 1))
  (handler-case
      (let ((*my-dyn* 2))
	(an-undefined-function))
    (undefined-function (f)
	(print *my-dyn*))))

;; The result is 1. That means we will have to wrap undefined function and hold the current
;; continuation
;; We cannot restart the undefined-function condition so using handler-bind and restarting
;; is not an option?
;; Solutions: modify the compiler in a dynamic way or wrap code

;; The remote call handler. We execute code on the other side using the
;; continuation in the raised exception

;; How do we know what piece of code execute on a remote call?
;; Option1 : same code in server and client. "Labeled" CPS code. Each
;; function call is encapsulated in a
;; Option 2: execute the current form and return immediately.
;; Option 3: pass code on the wire.
;; Option 4: make explicit client/server calls. Then we will know at compile time.

;; Sum in the server and display in the client, explicitly
(defun sum-action ()
  (let
      ((*k* (lambda (n1)
	      (let
		  ((*k* (lambda (n2)
			  (let
			      ((*k* (lambda (v)
				      (display "Sum result:~A~%" v))))
			    ;; Note that I have n1 in the lex env ;)
			    ;; Cause n1 is "from the past"
			    (funcall *k* (in-server (+ n1 n2)))))))
		(funcall *k* (rem-bind (prompt "Enter another number")))))))
    (funcall *k* (rem-bind (prompt "Enter a number")))))

;; We have problems with stack reification. What we want is to achieve that
;; through lexical transormations so that we can reify the stack in any language
;; (i.e. JavaScript)

;; So now the current continuation is represented by two parameters.
;; First, the closure. Second, the serialized closure.

;; To reify stack we need static/lexical analysis to know when to push
;; a new frame to the stack. And we need assignments and bindings
;; wrappers in order to get the values.
;; Special forms: let, let*, defun, labels, flet, lambda, progv, declare. symbol-macrolet and macrolet
;; are expanded by the code walker, so there's no problem.

;; Stack reification
(defgeneric reify-stack (form))
(defmethod reify-stack ((var local-variable-reference))
  var)

(defclass setf-form (form)
  ((place :accessor place :initarg :place :initform (error "Supply the place"))
   (value :accessor value :initarg :value :initform (error "Supply the value"))))

(defwalker-handler setf (form parent env)
  (declare (ignore parent env))
  (destructuring-bind (nil place value) form
      (make-instance 'setf-form :place (walk-form place) :value (walk-form value))))

(defclass defun-form (form)
  (name :accessor name :initarg :name :initform (error "Supply the function name"))
  (args :accessor args :initarg :args :initform (error "Supply the function arguments"))
  (body :accessor body :initarg :body :initform (error "Supply the function body")))

(defwalker-handler defun (form parent env)
  (destructuring-bind (nil name args body) form
      (multiple-value-bind (parsed-lambda-list lambda-list-env) (walk-lambda-list args)
	(multiple-value-bind )
      (make-instance 'defun-form :name name :args args :body (walk-form body)))) ; Need to build a lex env from args here and pass it to walk-form body

(defgeneric reify-stack (form parent env))

(defmethod reify-stack ((form setf-form) parent env)
  (with-slots (place value) form
    (case (type-of place)
	  ('variable-reference
	   (let ((value (unwalk-form value)))
	     (walk-form
	      (once-only (value)
		 `(progn
		    (set-env-var ,(name place) value)
		    ,(unwalk-form form)))
	      parent env)))
	  (t form))))

;; The problem with this version is with the walking environment
;; We have to use walk-lambda-list insted (see below)
(defmethod reify-stack ((form defun-form) parent env)
  (with-slots (name args body) form
    (multiple-value-bind
	  (required-args
	   optional-args
	   rest-arg-p
	   rest-arg
	   key-args-p
	   key-args
	   allow-other-keys-p
	   aux-args-p
	   aux-args
	   more-arg-p
	   more-arg
	   more-count-arg
	   lambda-list-keyword-p)
	(sb-int:parse-lambda-list-like-thing args)
      (when more-arg-p
	(error "Don't know how to handle a &more arg"))
      (when aux-args-p
	(error "Don't know how to handle &aux args"))
      (make-instance 'defun-form
		     :name (name form)
		     :args (args form)
		     :body (walk-form
			    `(progn
			      ,(loop for arg in (concatenate 'cons required-args
							    (loop for arg in optional-args
							       if (atom arg) collect arg
							       else collect (cadr arg)
							       end)
							    (loop for arg in key-args
							       if (atom arg) collect arg
							       else collect (cadr arg) end))
				 collect `(def-env-var ',arg ,arg))
			      ,@(unwalk-form (body form))))))))

(defmethod reify-stack ((form defun-form) parent env)
  (with-slots (name args body) form
    ;; I need to rewalk in order to get the appropiate environment :S
    ;; Question: how do I know the envionment returned by walk-lambda-list is enough
    ;; to evaluate the function body? If it is not, then I'll be force to work at the walking level
    ;; Other option: work at the walking level ?
    (arnesi::multiple-value-bind (lambda-list-args lambda-list-env)
	(walk-lambda-list (unwalk args) parent env)
      (make-instance 'defun-form
		     :name (name form)
		     :args (args form)
		     :body (walk-form
			    `(progn
			      ,(loop for arg in
				  (loop for arg in lambda-list-args
				       if (one-of ('required-function-argument-form
						        'optional-function-argument-form
							'keyword-function-argument-form) (type-of arg)
						  collect ()
				       collect (name arg)
				 collect `(def-env-var ',arg ,arg))
			      ,@(unwalk-form (body form))))
		     parent lambda-list-env))))


(defvar *env* (list (make-hash-table)))

(defun def-env-var (var value)
  (destructuring-bind (current-stack . nil) *env*
    (setf (gethash var current-stack) value)))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;; Plan to do code transformations at the walking level:
;; 1) Thread safety: place a lock to access *walking-handlers*
(defvar *walking-handlers-lock* (sb-thread:make-semaphore :name "walking-handlers-lock" :count 1)) ;; This variable should be available at compile time!

;; walk-form redefinition
(let
    ((old-walk-form (symbol-function 'walk-form)))
  (defun walk-form (form &optional (parent nil) (env nil))
    (sb-thread:wait-on-semaphore *walking-handlers-lock*)
    (funcall old-walk-form form parent nil)
    (sb-thread:signal-semaphore *walking-handlers-lock*)))

;; Copying a hash table
(defun copy-hash-table (orig)
  (let ((copy (make-hash-table
                ;; Specify size so that the table is redundantly
                ;; grown every time you add one or a few elements.
                 :size (hash-table-size orig))))
    (flet ((copy-key (key val)
             (setf (gethash key copy) val)))
      ;; Declare this function dynamic-extent so it doesn't make
      ;; closures in the heap.
      (declare (dynamic-extent #'copy-key))
      (maphash #'copy-key orig)
      copy)))

;; 2) Modify walking handlers non destructively
(defmacro preserving-handlers (&rest body)
  ;; Keep the previous handlers
  (let ((old-handlers (gensym)))
    `(progn
       (sb-thread:wait-on-semaphore *walking-handlers-lock*)
       (let ((,old-handlers (copy-hash-table *walking-handlers*)))
	 ;; Replace the handlers
	 ,@body
	 ;; Restore the handlers
	 (setf *walking-handlers* ,old-handlers))
       (sb-thread:signal-semaphore *walking-handlers-lock*))))

;; New thought: we don't need the parent and environment to do the transformation!
;; They are needed by code that is not our own, but that was already walked with parent
;; and environment. Our code doesn't need them as long as we use *already walked* code to
;; generate the new code. Disadvantage: we cannot unwalk and walk code again to build new forms
;; from templates because we need the parent and environment; we will have to modify
;; the tree directly.

;; Example of walking: cl-stm places proxy-forms containing the original walked code
;; forms and perform the transformations in the unwalking.

(preserving-handlers
 (defwalker-handler setf (form parent env)
  (declare (ignore parent env))
  (destructuring-bind (nil place value) form
      (make-instance 'setf-form :place (walk-form place) :value (walk-form value))))

 (defwalker-handler defun (form parent env)
  (destructuring-bind (nil name args body) form
    (multiple-value-bind (parsed-lambda-list lambda-list-env) (walk-lambda-list args)
      (multiple-value-bind )
      (make-instance 'defun-form :name name :args args :body (walk-form body)))))) ; Need to build a lex env from args here and pass it to walk-form body

;; Think: a new code walker
;; Differences:
;; 1) Extensible (dynamic binding, first-class functions)
;; 2) One-pass (lazy evaluation?)
;; 3) Composable (lazy evaluation?)

;; Designs:
;; 1) Do nothing. The current design is extensible. If you want to change the walker,
;;     change the definition of the relevant operations.
;;     This approach is extensible, one-pass, difficult to compose.
;;     See preserving-handlers macro.
;; 2) Keep the environment in the node. We have n-pass transformations, but the
;;     environment and thus templating is always available. Provide an API for defining
;;     new transformations.
;;     This approach is n-pass, extensible, composable.
;; 3) MOP approach: define hooks and hook-definers where we think it makes sense
;;     doing a transformation. Think of possible transformations in advance in order
;;     to come up with the hooks. When the MOP is not enough, we will need 1) or 2)
;;     This approach is extensible, one-pass, composable for general cases.
