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

Idea:
-----

Make constructs related to the stack work with component-chain (instead of the stack-chain). Example constructs: unwind-protect, handler-case, handler-bind, dynamic-variables, etc. We may use a combination of cps transformation plus some manual dynamic-environment handling (see contextl).

Implementation ideas:
First approach: Convert to continuation passing style and manipulate the dynamic-environment (keep them both and activate them when the user responds).
Second approach: Rewrite dynamic-binding operators in the component lambdas to follow the component chain instead of the stack frames chain.

The second approach looks easier, but I don't know.

Idea:
----

Implement component-wrappers (should be the equivalent of :after :before :around and other method combinations for methods).

component-wrappers should alter the behaviour of a component (for example, its rendering), but should remain transparent to some machinery (for example, the templating engine assignment policy should be independent of component-wrappers. component-wrappers are invisible to the template-engine).

Use case:

We can use component-wrappers to build complex objects editors. In general, an object editor should not contain an accept and cancel button, because that depends on the context. So, the following is incorrect:

(defcomponent person-editor ()
  ((name :type :input :model (name model))
   (lastname :type :input :model (lastname model))
   (accept :type :button :on-click (accept-edition self))      <-- The accept button should not belong to the editor
   (cancel :type :button :on-click (cancel-edition self))))    <-- The cancel button should not belong to the editor

The problem with this design is that it is difficult to build more complex editors from existent ones.

(defcomponent artist-editor ()
  ((artistic-name :type :input :model (artistic-name model))
   (art :type :input :model (art model))
   (accept :type :button :on-click (accept-edition self))      <-- The accept button should not belong to the editor
   (cancel :type :button :on-click (cancel-edition self))))    <-- The cancel button should not belong to the editor

The problem is that person-editor and artist-editor are not incompatible by design, but we are adding the accept and cancel button twice.

A possible solution is to use component-wrappers:

(defcomponent-wrapper editor-wrapper ()
  (:render (component wrapper)
	   (call-next-method)    ; We render the component
	   ; We render the accept and cancel buttons once.
	   (accept :type :button :on-click (accept-edition self))
	   (cancel :type :button :on-click (cancel-edition self))))

(defmethod call ((wrapper editor-wrapper))
  ; We wrap the editor call
  (with-transaction
      (call (component wrapper))))

And with transaction should expand to something similar to:
(unwind-protect
     (tagbody edition-block
	(restart-case (progn
			(begin-transaction)
			(call (component wrapper))
			(commit-stm-transaction))
	  (retry-edition ()
	    :report (lambda (stream)
		      (format stream "Restart the edition"))
	    ;; This is all. It's responsibility of the piece of code that
	    ;; throws errors to provide other restarts, such as :continue, for example.
	    (rollback-transaction)
	    (go edition-block))
	  (abort-edition ()
	    :report (lambda (stream)
		      (format stream "Abort the edition"))
	    (rollback-transaction))))
  (when (transaction-active)
    (rollback-transaction)))


THIS IS WHY WE NEED TO ADAPT DYNAMIC LANGUAGE CONSTRUCTS TO COMPONENT CHAINING!!

Finally, under this scheme, the editor component should commit anything on accept, and raise a signal (versioning-error, etc) on error.

Nested component transactions semantics:
---------------------------------------

If the inner transaction commits, then nothing happens. All the changes are commited iff the top level transaction commits.
If the inner transaction raises an error, or rollbacks, the outer transaction remains untouched. The inner transaction can be retried, and the computation resumes. That's why we have nested transactions; a nesting of transaction doesn't form a new bigger transaction, but the transactions hierarchy is preserved.
If the outer transaction rollbacks, then al changes are discarded, including changes made in inner transactions.
If the outer transaction commits, all the inner transactions must have been commited. If one of them is uncommited, then an error is raised (restart with the option to commit the remaining ones). If all of them are commited, then the outer transaction commits too, and all of the changes are made effective.
Once more, errors should be propagated through the component chain (not through the stack-chain).

Component calling and answer semantics:
---------------------------------------

When an active component calls another, it loses its control and passes it to the called component. The called component becomes the active one.
If an unactive component answers, then an error is raised (although proper restarts are available).
If an unactive component calls another, then an error is raised (although proper restarts are available).

If a child component calls another, then it loses focus, and the called one gains control.
If a child component answers an object, then it desappears from the screen. The parent can set a callback on it to intercept the child component answer.
child components multiply the flow of control. Continuation passing doesn't hold anymore in their presence. Example:

(defmethod initialize :after ((component my-component) &rest initargs)
  (declare (ignore initargs))
  (add-child (component first-child)
   (format t "This is the first flow of control")
   (let ((answer (call (make-instance 'my-child-component)))) ;; This embeds and sets the child component
     ;; The answer
     (format t "The first child component answered ~A" answer)))
  (add-child (component second-child)
   (format t "This is the second flow of control")
   (let ((answer (call (make-instance 'my-child-component)))) ;; This embeds and sets the child component
     ;; The answer
     (format t "The second child component answered ~A" answer))))

Child "components":
------------------

Wow!! add-child doen't take a component, it is a macro and takes a block of code (that corresponds to the concurrent control flow). It is the fork equivalent!! Great...

Sketchy implementation:

(defmacro add-child (component slot &rest body)
  (once-only (component)
	     `(setf (slot-value ,slot ,component)
		    (make-instance 'child-component-handler
				   :name (symbol-name ,slot)
				   :parent ,component
				   :go (lambda (self)
					 ,@body)))))

(defmethod call ((self child-component-handler) other-component &rest args)
  "call redefinition for child components calling"
  ;; set which is the child component (we need that, the template system needs to know which are the child components, for example)
  (setf (component self) component)
  ;; invoque the original call operation
  (prim-call self comp args))

(defmacro simple-add-child (component slot child-component)
  "Adds a child-component without 'threading code' complications"
  `(add-child ,component ,slot
	      (call ,child-component)))

child-component-handler may be designed as a subclass of task as it has a :go lambda.
It is different from a task because we need to redefine the call method on it in order to indicate
which is the new child component in the slot.

tasks are components with a :go lambda.

(defclass task (component)
  ((go :initarg :go :accessor go)))

Note: task components or variations of them may act as component-wrappers and/or child-component-handlers. Think about that.

We can extend this idea of child "components" and consider "building the whole component tree from lambdas"

About "component tree from lambdas":
-------------------------------

We can build the component tree from lambdas. The semantics of some of the operations depends on what kind of component we are building.

For example, to declare the main component, we could have:

(defapplication my-application ()
  ())

(defvar *application* nil "The running application")

;; We use defmethod/cc. We need continuations for component calling, and dynamic environment restoring (contextl) so that dynamic variables and
;; other structures continue to make sense in presence of continuations.

(defmethod/cc start-application :around ((app application))
   (let
       ((*application* app))
     (dflet ;; dynamic functions binding
         ((call (component)  ; the semantics of the call operation depends on the context (in this case we are setting the root component)
	      (set-root-component *application* component)))
	 (call-next-method))))

(defmethod/cc start-application ((app my-application))
  ;; now, this is an example of how we can specify the root component of our application
  (loop while t
       do (let ((user (call (make-instance 'login-component))))
	    (set-logged-user user)
	    (call (make-instance 'main-component))
	    (unlog-user))))

Other example, is the semantics of call when adding child components.


        