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

Context oriented programming
----------------------------

Once dynamic language constructs are adapted, we can start to use context oriented features for our application.

Example:

(deflayer listing-layer () ())

We can layer the controller behaviour:

(def-layered-component :layer listing-layer person-viewer (viewer))

(def-layered-method :layer listing-layer initialize ((viewer person-viewer))
		    ...)

After that we can do:

(add-child
 (with-active-layers (listing-layer)
   (call (make-instance 'persons-component))))

It is also possible, and may make sense, to make the template engine context aware:


<template class="person-viewer"
          layer="listing-layer">
   ...
</template>

Application navigation and bookmarking:
---------------------------------------

The bookmarking problem is difficult to solve in the context of a complex Web application. In particular, it is not clear how to obtain a particular state of the application from a simple bookmarking string. Besides, there are some states of the application that cannot and should not be reached by bookmarking. That's the case of a user session navigation (content that is available to a logged user only) or a commercial transaction, for example. So, first, we must identify the parts of the application that are reachable by bookmarking. Second, we must provide a framework that allows us to do that as simply and naturally as possible.

One approach to solve the problem, would be to map the application as a state transition machine, identify the navigational paths, and apply a tag to each one of them. But we've already seen that determine each of the states is not desirable in complex Web applications. So we'd better find a better approach.

Discarding the state machine, there are at least two more possibilities to solve the problem:

One would be to register the components calling chain. The advantages are: we can repeat the navigation chain to reach any state. The disadvantages are: the chain of components gets large depending of how much time the user has been navigating. That is to say, how many transitions the user caused. On the other hand, there's room for a lot of unnecesary applications transitions; a same state could have been reached with less transitions. This is because this solution is not mapping a multiple input (components transition) to a unique output (a declarative application state specification). [See the next section for a generalized explanation of this problem].

The other option is to register pertinent navigation layers and component states to reach the desired state. Bookmarks have a declarative taste, like this.

bookmark = layers=layer1:layer2+collection-navigator-1:offset=22:segment=22... etc

Each component should define how to be restored given some parameters. Some components may be uninteresting to restore, once more, depending on the context.

Bookmarking configuration: we can have three ways of configuring a component.

1) From the component itself (subclasses, mixins, class definition).
(defcomponent collection-navigator ()

  (:bookmarking (offset :accessor bm-offset) ;; bm-offset is used to extract the offset in bookmarking format. (setf bm-offset) is used to set the component offset from a bookmarking parameter)
		(segment :accessor bm-segment))
  (:bookmark :all))

2) From the outside.
(with (make-instance 'collection-navigator :on my-collection)
      (disable-bookmarking-of 'segment it)
      (call it))

3) From the outside, dynamically. It may be useful if we don't want to bookmark certain embedded subcomponent nor any of its components.
(with (make-instance 'my-complex-component)
      (disabling-bookmarking
       (call it)))


Pattern: Mapping multiple inputs to a single output
---------------------------------------------------

This is a pattern that arises in several contexts.

Problem: executing multiple inputs is a waste, and some times produces incorrect behaviour (although the same functional output, repeating a same input multiple times may produce a difference in side effects).

Examples:
1) Dataflow. A some state change(s) may provoke several changes to a same cell. The problem is, the same output could have been achieved deferring the changes, analizing them and updating the cells only once. A clear example: when adding several elements to a collection. Whenever the collection changes, the change is propagated to the view, so that it gets updated. The problem is that if we add or remove several elements to a collection, the view is updated as many times as operations made on the collection. This is undesirable, because the application is doing unnecesary work. What we want is to execute the changes on the collection transactionally; the view gets updated once all the operations are done.

2) Templates tree modification. Repeated changes on the same part of the tree, results in only one change, depending on the state of the previously rendered tree.

3) Bookmarks generation. We could generate bookmarks rendering the component calling sequence. The problem is that although we can reach almost any navigationaly interesting application state doing this, we would be doing unnecesary work once more. The same state could be reach with less state transitions, probably. So the solution is to achieve a declarative bookmark specification somehow, from the components states and some navigational flow information (i.e. context layers), to achieve the same result.

Solution: defer side effects and analize inputs consecuences and produce a "normalized" output, where the minimal of inputs is executed to produce the desired output.




Modal dialogs
-------------

This about what it means to have modal dialogs, etc.

(with (make-instance 'message-box :text "Some information")
      (modally
          (call it)))

(defvar *modal-call* nil "If the affected call is to be made modally")

(defmacro modally (&rest body)
  `(let
       ((*modal-call* t))
     ,@body))

The call operation should look at the *modal-call* variable and act upon it.

Components threads semantics:
-----------------------------

Child threads are aborted if the parent is aborted. For example, if the user hits the logout button of the root component, all of the child components are aborted before aborting the root component.

So, for example:

(defapplication my-application ()
  ())

(defmethod/cc start-application ((app my-application))
  ;; now, this is an example of how we can specify the root component of our application
  (loop while t
       do (let ((user (call (make-instance 'login-component))))
	    (set-logged-user user)
	    (call (make-instance 'main-component)) ; **
	    (unlog-user))))

** When the user hits the logout button of the main-component, the main-component child threads are aborted before proceeding. Note that aborting a child component may lead to some behaviour. For example, if the child component is an editor that is configured to ask for cancellation in case the user leaves the component. Then, trying to logout, will raise the cancelling exception and a dialog box asking for cancellation will appear; the user will not be able to logout without asking. Now, that's ok, but it would be interesting to think how we can control that. For example, we may want the editor to avoid asking the question in case we hit the logout button, but ask it otherwise. It's not clear how to achieve that with a "threading" semantics. Possible solution??: the logout button activates a layer. That layer should deactivate the abort-condition catch up that shows the dialog, and provide some that just proceeds with the editor abortion. Not trivial anyway...but interesting.

Not that the components threads semantics is preemtive; the parent component may abort nested components.

Implementation thought: we may implement dynamic environments manual setting using ContextL dynamic-environments manipulation library. Then, each component would hold its own dynamic layer (or environment). The user should be able to control whether he wants to restore some dynamic environments or not. In the logout case, we don't want to restore the aborted component (editor) dynamic environment; we want to treat signaled conditions differently (for example, avoiding a question dialog, and proceeding instead). If the user leaves, or hits cancel, then we *do* want to restore the components dynamic environment.

Sketch:

Suppose we have A as the parent of B. B calls C for performing some operation.

(defmethod initialize-instance :after ((comp A) &rest initargs)
  (add-child comp
	...     
   (call comp 'B)
     ...
   ))

;; add-child should get translated to the following:
(let ((env (dynamic-environment comp)))
  (with-dynamic-environment env ;; the parent component dynamic-environment
    (dynamic-wind     ;; This *should* compose the local environment with the above one
     ...
     (proceed
      (let ((child (make-instance 'B)))
	(setf (environment child) (capture-dynamic-environment))
	(effectively-add-child child)))
     ...)))

(defaction accept-action ((comp B))
  ...
  (call 'C)
  ...)

; should be translated to something like:
(let ((env (dynamic-environment comp)))
  (with-dynamic-environment env ;; the parent component dynamic-environment
    (dynamic-wind     ;; This *must* compose the local environment with the above one
     ...
     (proceed
      (let ((calle (make-instance 'B)))
	(setf (environment calle) (capture-dynamic-environment))
	(effective-call calle)))
     ...)))

;; Note:
;; First: the code before and after the call (...) is part of the dynamic winding (altough if we have continuations "there's no after code").
;; Second: with-dynamic-environment pushes the reevaluated thunks to the stack so we get an augmented dynamic environment when we call capture-dynamic-environment. So, now we have component threads dynamic-environments. So, for example, component threads dynamic variables can be accesed like this:

(defdynamic my-var 33)
(defdynamic parent-var 22)

;; Luego podemos acceder esa variable con (dynamic my-var) en el scope generado a través de la inyección de environments en los componentes:

(defmethod initialize-instance :after ((comp A))
  (add-child comp
    (dynamic-let
       ((parent-var 45))
         (call 'B))))

(defaction accept-action ((comp B))
  (dynamic-let
    ((my-var 55))
      (call 'C)))

(defmethod initialize-instance :after ()
    (print (dynamic parent-var)) ;; This prints 45!! (component thread variable!)
    (print (dynamic my-var))) ;; This prints 55

