#| Stratified design again:

First, we should be able to ouput js.

First level, a function returns a javascript string.
Second level, parenscript.
Third level, transparent ajax and comet updates.

AJAX updates design:
First level, update remote parts of the html at the text replacement level (manual ids). Ala, Seaside:
...   :live-callback (:update
                             "my-div-id" :with (format nil "<spam>~A</spam>" my-text)

or

... :live-callback (:update "my-div-id" :with (html (:spam (str my-text)))
                                        "my-other-div-id" :with (html (:spam "Hello!")))

Besides, we have several other possibilities apart from updating (replacing in content). We can replace the element itself through :replace, remove elements, etc

Example

...  :live-callback ((:remove "my-div-id")
                            (:replace "my-other-div" :by (html (:div :id "new-div" (:p "holaa"))))
(:insert-before "div22" (html (:div :id "div21" (:p "inserted")))))

The second level binds first class dom-objects:

(let ((my-view (html* (:div (:p "hello")))))
  (... :live-callback ((:replace my-view :by (html* (:div :id "my-other-div" (:p "byeeee"))))))
  )

When building a tree with html* the ids are automatically generated when rendered so that we can later access its elements later with variable bindings like in the example above or navigating the tree (there should be some way of )

html* builds a tree object

ATENTION: we need to really build a TREE instead of working with a STREAM to do this. Besides, it seems so much more reasonable and adapts a lot better to dataflow manipulations. On the other hand, strems and trees don't look compatible, although we may first convert a tree to a stream and then use it when needed.

When building a tree or stream, we should be able to use XML reader syntax:

(let ((my-view x#(<div id="my-div"><p>hello</p></div>)))
  (... :live-callback ((:replace my-view :by #x(<div id="my-other-div"><p>byeeee</p></div>))))
  )



TODO: investigate how UCW does AJAX

#|
Now we apply free-variables detection to build a javascript-server comunication library

(defcomponent my-object-deleter ()
  ()
  (:initialize ()
	       (let ((object (model self)))
		 (add-action-link :name "delete-object"
				  :default-display "Delete object"
				  :action
				  (client
				   ;; The free variable "object" is passed to the client
				   (if (open-dialog 'question-dialog :text (format nil "Are you sure you want to delete ~A?" object))
				       (server
					;; Now the free variable "object" refers to the client proxy. The client passes de object id
					(delete-object object))))
				)
	       )
  ))

;; Different identifyable lambdas should be created in both sides, in the client and the server. We assign an id to each of them and call them distributely (RPC)

;; For example, for the above example, we have two lambdas:

  In the client:
  (create-lambda (object)
		 (if (open-dialog 'question-dialog :text (format nil "Are you sure you want to delete ~A?" object))
		     (call-server :session 4 :id 44 :params object)))

  In the server:
  (create-lambda (object)
		 (delete-object object))

  And the resulting action is:
  (let ((object (model self)))
		 (add-action-link :name "delete-object"
				  :default-display "Delete object"
				  :action (call-client :session *session* :id 33 :params object)))

  Besides, the javascript to create lambdas is dynamically transferred when the component is active
		
  |#

(defclass component ()
  ((children :accessor children :initform '()))
  )

(defclass widget ()
  ()
  )

(defclass action-link (widget)
  ((name :accessor name :initarg :name)
   (default-display :accessor default-display :initarg :default-display)
   (action :accessor action :initarg :action)))

(defvar *server-entry-counter* 1)
(defvar *server-entries* (make-hash-table :test #'equal))

(defvar *client-entry-counter* 1)
(defvar *client-entries* (make-hash-table :test #'equal))


(defun register-server-entrypoint (exp)
  (let ((entry-id *server-entry-counter*)
	(freevars (list-free-vars (list exp))))
    (incf *server-entry-counter*)
    (values
     entry-id
     freevars
    `(setf (gethash ,entry-id *server-entries*)
	  (lambda ,freevars ,exp)))))

(defun register-client-entrypoint (exp)
  (let ((entry-id *client-entry-counter*)
	(freevars (list-free-vars (list exp))))
    (incf *client-entry-counter*)
    (values
     entry-id freevars
     `(setf (gethash ,entry-id *client-entries*)
				     (lambda ,freevars, exp)))))

(defvar *in-server-context* t)
(defvar *client-lambdas* '())
(defvar *server-lambdas* '())

(defun process-action (code)
  (labels
	((process-tree (exp)
	   (if (atom exp)
	       exp
	       (let ((operation (car exp))
		     (args (cdr exp)))
		 (case operation
		   (server
		     (if *in-server-context*
			 `(progn
			    ,@(mapcar #'process-tree args))
			 (call-server `(progn ,@args))))
		   (client
		    (if *in-server-context*
			(call-client `(progn ,@args))
			`(progn
			   ,@(mapcar #'process-tree args))))
		   (let
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(let
			      ,(loop for binding in bindings
				  for form = (cadr binding)
				  collect
				    (list (car binding) (process-tree form)))
			    ,(process-tree `(progn ,@body)))))
		   (let*
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(let*
			     ,(loop for binding in bindings
				 for form = (cadr binding)
				 collect (list (car binding) (process-tree form)))
			 ,(process-tree `(progn ,@body)))))
		   (flet
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(flet
			      ,(loop for binding in bindings
				  for fbody = (nth 2 binding)
				  collect (list (car binding) (cadr binding) (process-tree `(progn ,@fbody))))
			    ,(process-tree `(progn ,@body)))))
		   (labels
		       (let
			   ((bindings (car args))
			    (body (cdr args)))
			 `(labels
			      ,(loop for binding in bindings
				  for fbody = (nth 2 binding)
				  collect (list (car binding) (cadr binding) (process-tree `(progn ,@fbody))))
			    ,(process-tree `(progn ,@body)))))
		   (return-from
		    (error "Unimplemented"))
		   (t
		    `(,operation ,@(mapcar #'process-tree args)))))))
	 (call-server (code)
	   (let ((*in-server-context* t))
	     ;; Register a server-entry-point from the code
	     (multiple-value-bind (id parameters lambdas-code)
		 (register-server-entrypoint (process-tree code))
	       (push lambdas-code *server-lambdas*)
	       ;; Return the code that calls it
	       `(server-call ,id ,@parameters))))
	 (call-client (code)
	   (let ((*in-server-context* nil))
	     (multiple-value-bind (id parameters lambdas-code)
		 (register-client-entrypoint (process-tree code))
	       (push lambdas-code *client-lambdas*)
	       ;; Return the code that calls it
	       `(client-call ,id ,@parameters)))))
	 (process-tree code)))

(defun process-action-2 (code)
  (let
      ((*in-server-context* nil)
       (*client-lambdas* '())
       (*server-lambdas* '())
       (*client-entry-counter* 1)
       (*server-entry-counter* 1))
    (let ((action-code (process-action code)))
      (values action-code
	      `(progn ,@*server-lambdas*)
	      (compile-script `(progn ,@*client-lambdas*))))))

;; Test

;; (process-action-2 '(progn (let ((chau "chau")) (client (print "hola" obj) (server (print "hola" chau) (client (print "hi")))))))

(defmacro add-action-link (c &key name default-display action)
  (multiple-value-bind
	(action-code server-lambdas client-lambdas)
      (process-action-2 (macroexpand action))
    `(progn
       (push (make-instance 'action-link :name ,name :default-display ,default-display :action (lambda () ,(compile-script action-code))) (children ,c))
       ,server-lambdas
       ,client-lambdas)))

#| Test:


(add-action-link my-component
		 :name "my-component"
		 :default-display "My Component"
		 :action (progn (print "hola" obj) (server (let ((chau "chau")) (print "hola" chau) (client (print "hi") (print chau))))))


Problema con esta version de add-action-link: es estrictamente necesario hacer una conversión CPS por cada operacion server y client. Esto es así ya que, en el caso de un rendering standard, el código a continuar debe ir en un atributo onLoad del tag body. En el caso de Ajax, el codigo a continuar debe ir de forma similar atacheado a un objeto HttpRequest en forma de callback. Una particularidad de esta conversión es que debe tener en cuenta las variables libres para saber exactamente que variables vamos pasando del servidor al cliente y del cliente al servidor (no tenemos ni un stack reificado (interprete) ni un stack implícito con bindings en memoria (lambdas + frames))

More documentation:

Javascript stratified design
----------------------------

1) Javascript generation
a) String concatenation
b) Macros (parenscript)

2) Client-server communication
a) Remote function calling. RPC, JSON representation of objects.
    Client: callServer('serverProc1', 2, 'hola', {'key': 45}, ...)
   That is to say, manual remote function calling with basic data types transmition.
    Server: (server-response "doSomething" 45 "hello" '(23 45))

  Besides, there may be predefined functions for common tasks.

b)Remote objects. Objects proxies.
  Client proxy codification.
  Server object codification.
  Client proxy and server object synchronization codification (manual. provide some API to do that. Define a design)

3) Client-server syntax:
   (let ((map (make-instance 'google-map)))
     (add-component self map)
     (client
        (map.on-click (lambda (location)
              (map.move location)
                 (server
                   (trigger-event map "changed"))))))

In this example, the map variable refers to an object proxy in the (client ...) form, and to the server object (real object) in the (server ...) form. The object must be kept synchronized (for example, when the location is changed in the client through the move method, that change is transmited and is observable in the server, that triggers the event)

4) Proxies synchronization programming through MOP

(defclass gmap ()
  ((location :update t))
(:metaclass 'jsproxy-class))

(defproxymethod move ((map gmap) location)
  (setf (location map) location))

...

5) Maybe as a higher level or replacing the former, it could make sense having simple dispatch and automatic client server code position.

Example:


(let ((map (make-instance 'google-map))
       (add-component self map)
       (set-event map :click (lambda (location)
         (trigger-event map :changed)))))

But this design looks more difficult to achieve.



Some comments on event handlers attaching and events firing:
* An event's firing may imply an AJAX request, but it may be innecesary if there's no event handler attached in the server. So, first,
  the AJAX request should be made when there are event handlers attached in the server. Second, the event handler attaching in the server,
  generates javascript, so that attaching should be made in a javascript rendering context. That means, the events attaching can only
  be made on the initialize method of the component, or in a callback context.

Some comments on javascript generation and integration:

* Javascript should be handled in a compositional way. That means, each component depends on js-libraries that are loaded on demand, and besides, may generate its own javascript code.
* There are tricky issues when generating server calling code inside a loop. Example:

(defcomponent mygmap (gmap)
  ()
  (:metaclass js-component)
  (:initialize-map (self)
		   ;; The following is code that modifies the mymap server object state and generate javascript
		   ;; to modify the actual component in the client, at the same time.
		   (loop for i from 1 to 10
			do (let
			       ((marker (make-instance 'gmarker :lat (random :between (west (bounds self))
									                                     :and (east (bounds self)))))))
			(on-click marker (lambda (event)
					   (alert (format nil "Clicked on ~A" marker))
					   (self.show-info-window :message
								  (server (let ((msg (message-for-marker marker)))
									    (as-html msg)))))))))
|#