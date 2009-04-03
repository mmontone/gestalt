(def (action delete-object-action) (object)
  (if (call (make-instance 'question-dialog :question "Delete the object?"))
      (progn
	(delete-object object)
	(call (make-instance 'notification-dialog :notification "The object was successfully deleted"))
	(return object))
      (return nil)))

; If the javascript backend is active, then I want to compile as much as I can on the client (unless the user specified to compile in the server somehow)
; Solution one: walk the code and check which operations are available on the client and which ones not.
; That separates the code in client and server.
; In this example, the separated code would be:

;; Client
(def (client-action delete-object-action) (object)
  (if (call (make-instance 'question-dialog :question "Delete the object?"))
      (progn
	(in-server (delete-object object)
		   (progn
		     (call (make-instance 'notification-dialog :notification "The object was successfully deleted"))
		     (return object))))
      (return nil)))

;; in-server invoques the delete-object function through rpc serializing object and waits til the request finishes
(in-server (delete-object object)
	   (progn
	     (call (make-instance 'notification-dialog :notification "The object was successfully deleted"))
	     (return object)))

;; generates the following code:

(rpc-server-call 'delete-object :args (serialize object)
		 :callback (lambda (object) 
			     (call (make-instance 'notification-dialog :notification "The object was successfully deleted"))
			     (return object)))

;; Compilation order:
;; Solution 1: declare explicitely which definitions are meant to be in the client when possible
(def (client-action delete-object-action) (object))

;; Solution 2:  deferred compiling. Two fase compiling. First, read definitions and keep them in memory. Declare them all client-actions.
;; If an action cannot be compiled on the client, then declare it server-side and compile all definitions again.

;; Explicit placed code: sometimes similar code is available in the server and in the client; but we may prefer one over the other. We can
;; execute some code explicitely with (in-server)
;; Example: + is available both in the server and the client, but we prefer doing it in the server.
(def (action sum-up) ()
  (let ((n1 (call (make-instance 'prompt-dialog :message "Enter a number")))
	(n2 (call (make-instance 'prompt-dialog :message "Enter another number"))))
    (return (in-server (+ n1 n2)))))

;; in-server forces the sum to be executed in the server

;; note we need some kind of cps on the client too. For example, the above sum-up definition should compile to something like this:

(def (client-action sum-up) ()
  (funcall
      (lambda (k)
	(def-local-var n1 (call (make-instance 'prompt-dialog :message "Enter a number")))
	(funcall k (n1))) ;; The one who really does this call is the prompt-dialog when it answers!!
    (lambda (n1)
      (funcall
	  (lambda (k)
	    (def-local-var n2 (call (make-instance 'prompt-dialog :message "Enter another number")))
	    (funcall k (n2)))
	(lambda (n2)
	  ;; Note that I have n1 in the lex env ;)
	  ;; Cause n1 is "from the past"
	  (in-server (+ n1 n2)))))))

;; Wow!! It seems there are things to do!!

;; I wouldn't like continuations to be compulsory
;; Then we should implement a callback mechanism based on closures

(def (action sum-up) ()
  (call (make-instance 'prompt-dialog :message "Enter a number")
	:on-answer
	(lambda (n1)
	  (call (make-instance 'prompt-dialog :message "Enter another number"))
	  :on-answer
	  (lambda (n2)
	    (return (in-server (+ n1 n2)))))))

;; This code does not require continuations, but is a lot uglier

;; Libraries:
;; parenscript - JavaScript generation
;; jwacs - JavaScript continuations
;; paren-psos - parenscript CLOS support
;; ucw - server side components support
;; scriptaculo.us DOJO - widgets toolkit
;; cl-cont - CPS compiler
;; cells - Dataflow support - Leave for later as an extension

;; Think:
;; 1) server and client control-flow synchronization?
;; 2) declaring explicit client components

;; Explicit client components:
;; Some components may be declared client-side-only or server-side-only. Maybe some aspect oriented (separate) declarations (dynamic binding when compiling).
 
;; Server and client control-flow synchronization
;; COOL IDEA: pass a stack around!! Do it conservatively, but may contain certain dynamic variables, serialized continuations, etc
;; That way we can simultate being in one machine to a certain extent.
;; Implementation: pass only the stack *changes*, not the whole stack.

;; Think of how to handle the net delay and failures
;; Probably handle in an aspect oriented way. Separate it from the interesting code. Place failure handling in a meta code.

;; Think: how different may "splitting compilation" be from the message passing approach for distributed computation?

;; Think: are in-memory views viable under this architecture?

;; How does this differ from JavaScript encapsulation (ala Java Wicket)?
;; 1) Navigation "problems" are not solved. We still need to access the server for navigational issues.
;; 2) Customization and extension problems. We have to program JS in the bear metal.
;; 3) Compatibility problems. JavaScript compatibility problems with browsers.
;; 4) Constant two-fold thinking for client and server code.
;; 5) Manual objects sending?
;; 6) Not as fun ;)


;; Investigate X-Windows/NX GUI distribution!!


;; Optional sentences for client and server code compatibility (e.g. visual effects)
;; Suppose we want to have a tab component. We program it in lwt. That way the component
;; is available both server side or client side; that depends on the way we compile our code (in this case,
;; the component code). But we want tab transitions effects in case the component is compiled to the client.
;; In this case optional declarations would be useful in order not to oblige the component to be a client-only one.


;; Examples:

(defun switch-tab ((comp tab-comp) tab)
  (declare (optional transition-effect))
  ... switch the tab ...
  (transition-effect))

;; or

(defun :optional transition-effect ()
       ...
       )

(defun switch-tab ((comp tab-comp) tab)
  ... switch the tab ...
  (transition-effect))

;; other idea: use AspectL for that!!

;; Easy client and server interaction: apart from providing a in-server construct I would also like to handle events easily. For example, implementing
;; server push support when necesary. Or going to the server when necesary. I'm talking about an event system.

;; Example:
(def (client-action my-client-action) ()
  (when-server database-changed (lambda () (udpate-the-view))))

;; That could either be implemented with a client time out and polling. Or pushing through commet.

;; Example:
(def (server-action my-server-action) ()
  (when-changes selectable-box (lambda (selection) (add-selection-to-db selection))))