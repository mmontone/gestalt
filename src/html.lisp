;; When rendering javascript for a component, we need a "main script" per component, where we can declare our javascript global variables. Besides, those variables are accesible from other dependent components.

(defcomponent tracking-map ()
  ((map :component t :type 'gmap))
  
  )


(defcomponent recording-panel ()
  ((map :initarg :map :initform (error "Supply the map"))))

;In order to compose, javascript global variables should be generated with the help of (gensym) and a lisp global binding should be created so we can reference it accordingly later in the generated javascript code
(defmethod js-render ((self recording-panel))
  "The per-component global javascript code"
  "The global path"
  (defvar path (make-array))
  (add-event-listener path "changed"
		      (lambda ()
			; The path changed, we have to refresh the panel
			(remove self)
			(render self))))
			
  

(defmethod render ((self recording-panel))
  (:div :id "path-table"
	(:anchor :text "Click here to begin recording"
		 :on-click (:js
		      ; Hide the playing panel
		      (.hide ($ "playing-panel"))
		      ; Show the stop recording button
		      (.show ($ "stop-recording"))
		      (let ((map ($ "map-canvas")))
			(g-event.add-listener map
					     (lambda (overlay point)
					       ; We assume the path is a global array
					       ; In the expression below, path should reference the global variable delclared above
					       (if (path.is-empty)
						   "The path is empty, the first element to create is the from-lat-lng"
						   (let* ((row (make-element
							       (:tr (:td (:p point))
								    (:td (:a :on-click
									     (lambda ()
									       (path.remove row)
									       (trigger-event "changed" path)) "remove")))))
							 ; We may not be able to build the row above like that because there's a reference to it in the on-click handler inside. We try like this:
							 (anchor (make-element (:a "remove")))
							 (row2 (make-element (:tr :td (elem anchor)))))
						     (anchor.set-attribute :on-click (lambda ()
										       (path.remove row2)
										       (trigger-event "changed" path)))
						     (.append-child ($ "path-table") row))
						   ;; Else, the path is not empty
						   (let*
							((anchor (make-element (:a "remove")))
							 (row (js (:tr (:tr (:p point))
									(:td anchor)))))
						     (anchor.set-attribute :on-click (lambda ()
										       (path.remove row)
										       (trigger-event "change" path))))))))))))

;; The next refinement is to get rid of id references and reference html structures portions with variable bindings.
;; Pay attention to .append.child and $ forms

(defmethod render ((self recording-panel))
  (let ((path-table
	 (:div 
	  (:anchor :text "Click here to begin recording"
		 :on-click (:js
		      ; Hide the playing panel
		      (.hide *playing-panel*)
		      ; Show the stop recording button
		      (.show *stop-recording*)
		      (let ((map *map-canvas*))
			(g-event.add-listener map
					     (lambda (overlay point)
					       ; We assume the path is a global array
					       ; In the expression below, path should reference the global variable delclared above
					       (if (path.is-empty)
						   "The path is empty, the first element to create is the from-lat-lng"
						   (let* ((row (make-element
							       (:tr (:td (:p point))
								    (:td (:a :on-click
									     (lambda ()
									       (path.remove row)
									       (trigger-event "changed" path)) "remove")))))
							 ; We may not be able to build the row above like that because there's a reference to it in the on-click handler inside. We try like this:
							 (anchor (make-element (:a "remove")))
							 (row2 (make-element (:tr :td (elem anchor)))))
						     (anchor.set-attribute :on-click (lambda ()
										       (path.remove row2)
										       (trigger-event "changed" path)))
						     (.append.child path-table row))
						   ;; Else, the path is not empty
						   (let*
							((anchor (make-element (:a "remove")))
							 (row (js (:tr (:tr (:p point))
									(:td anchor)))))
						     (anchor.set-attribute :on-click (lambda ()
										       (path.remove row)
										       (trigger-event "change" path)))))))))))))

  #|

    Possible implementation:

1) Each html structure is assigned an id so that we have a way to reference it. The id is automatically generated (gensym). 
2) Each time we bind a variable through a let in our language, we put that variable name in a lexical scoped hash table (so that we take into account let nesting), together with the id of the bound structure.
3) We we reference a variable, that variable reference is replaced by (document.get-element-by-id (get-hash ,variable-name *structures*)), using symbol-macrolet probably

    |#

    ;; Another level of refinement is to have widgets:

    (defmethod render ((self recording-panel))
      (let ((path-table
	 (mk-container 
	  (mk-button :text "Click here to begin recording"
		 :on-click (:js
		      ; Hide the playing panel
		      (.hide *playing-panel*)
		      ; Show the stop recording button
		      (.show *stop-recording*)
		      (let ((map *map-canvas*))
			(g-event.add-listener map
					     (lambda (overlay point)
					       ; We assume the path is a global array
					       ; In the expression below, path should reference the global variable delclared above
					       (if (path.is-empty)
						   "The path is empty, the first element to create is the from-lat-lng"
						   (let* ((row (mk-table-row :data (list (mk-table-data (mk-label point))
											 (mk-table-data (mk-button :on-click
									     (lambda ()
									       (path.remove row)
									       (trigger-event "changed" path)) "remove")))))
							 ; We may not be able to build the row above like that because there's a reference to it in the on-click handler inside. We try like this:
							 (anchor (mk-button "remove"))
							 (row2 (mk-table (mk-table-data anchor))))
						     (anchor.set-attribute :on-click (lambda ()
										       (path.remove row2)
										       (trigger-event "changed" path)))
						     ...
						     
;; Of course, the same could be achieved providing new "html tags". Those may represent our widgets (like UCW does)

;; And another level of refinement would be to integrate that with client/server calling syntax, and dataflow support for automatic html structures updating.
;; In pwb, we had widgets and each widget is dependent on a model. We have to design dependent structures. ONE WAY COULD BE TO MAKE HTML STRUCTURES THAT REFERENCE FREE VARIABLES ARE MADE DEPENDENT ON THEM, but it's not clear to me how to do that.

;; Example: (html
			(:table
			 (:tr
			(for element in path
			     (:td (:p element))))))

	In this case, the for is a special form in our html language. Besides, element appears free in the :p structure, so that is structure is made dependent on them. Besides, path appears free in :tr, so it is made dependent on it. Now, whenever path changes (adds an element, for example), :tr gets redraw. Besides, whenever one of the elements changes, the corresponding :p is redraw to show the changes.

	;; One drawback of this is that, if an element is added at the end of path, the whole :table is redrawn. We can leave that to the for implementation. For example, :tr could be made dependent on path using "add-last" "remove-last events, so that it is able to act accordingly. That is to say, we don't have an imperative for; we transform it in a smart event attaching.

						     
						     

						     