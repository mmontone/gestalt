;; PROBLEMA: las continuaciones compiladas y el backtracking no se llevan bien. Esto es así ya que no es posible determinar cuales objetos a copiar, ni como. Además, las continuaciones compiladas en lambdas ya contienen los bindings a los componentes; no es posible cambiar estos bindings para que apunten a copias de ellos. Bajo un interprete, esto no es problema ya que los registros de activación están reificados; por lo tanto, es posible implementar continuaciones copiando estos registros de activación y los componentes y objetos a los que se apunta; no es necesario ninguna transformación de código. Sin embargo, es posible tener continuaciones compiladas y backtracking, al mismo tiempo, si podemos implementar las estructuras a backtrackear como estructuras de datos persistentes, más un protocolo de que partes se persisten y como a determinar por el usuario. Además, el esquema de estructuras persistentes es independiente de las continuaciones, por lo que el control de flujo puede implementarse de cualquier otra forma y seguir teniendo backtracking. Este diseño está mucho menos acoplado que el del uso de continuaciones y copia de registros de activacion.



;; Ahora veamos la implementación de nuestra estructura de datos persistente. Utilizaremos la técnica path-copy. Cada vez que se hace un cambio al árbol de componentes, este copia mediante una implementación path-copy. Los nodos son copiados mediante una funcion genérica: copy-node. Esta función puede especializarse para copiar los objetos del modelo pertinentes en caso de backtracking. De ahora en más, cada requerimiento deberá contener la versión del árbol de componentes que se va a utilizar. Como a priori no sabemos aquellas versiones que serán utilizadas (el usuario podría entrar cualquier url en un momento determinado), no es posible valernos solo de enlaces a los roots de las versiones y que estas sean garbage collectadas cuando no se requieran mas; es necesario indexar las versiones en otra estructura de datos para hacerlas permanentes.

(defclass node-mixin ()
  ((children :initform '() :accessor children :initarg :children)))

(defclass with-parent-node-mixin (node-mixin)
  ((parent :initform nil :accessor parent)))

(defclass value-node-mixin (node-mixin)
  ((value :accessor value :initarg :value)))

(defun not-rootp (node)
  (null (parent node)))


(defun gfoldl (next null data f a)
  "A foldl on generic datastrutures.
     next: the function to traverse the datastructure. Should give the next element.
     null: the function to tell if we have got to the base case.
     data: the data structure.
     f: the processing function. Takes the current node and the processed structure. Should give a new processed structure.
     a: the accumulated processed structure."
  (if (funcall null data)
      a
      (gfoldl next null
	      (funcall next data) f
	      (funcall f data a))))

;; Example:
(defun list-foldl (list f a)
  (gfoldl #'cdr #'null list f a))

(defun parent-path-foldl (node f a)
  (gfoldl #'parent #'not-rootp node f a))

(defun parent-key-path-foldl (node f a &optional (child-key nil))
  (if (not-rootp node)
      a
      (parent-key-path-foldl (parent node)
			 f
			 (funcall f node a child-key)
			 (child-key node (parent node)))))

(defgeneric copy-node (node-mixin))

(defmethod copy-node ((node node-mixin))
  (let ((new-node (allocate-instance (class-of node))))
    (setf (children new-node) (children node)
	  (parent new-node) (parent node))
    new-node))
  
(defmethod copy-node :around ((node value-node-mixin))
  (let
      ((new-node (call-next-method)))
    (setf (value new-node) (value node))
    new-node))

;; node-children-indexing-mixins
(defclass node-children-indexing-mixin () ())
(defclass hash-node-children-indexing-mixin (node-children-indexing-mixin)
  ())

(defmethod initialize-instance ((node hash-node-children-indexing-mixin) &rest init-args)
  (call-next-method)
  (setf (children node) (make-hash-table))
  (let ((children-list (getf init-args :children)))
    (loop for cons-or-child in children-list
       do (if (consp cons-or-child)
		(set-child node (car cons-or-child) (cdr cons-or-child))
		(add-child node cons-or-child)))))


(defgeneric set-child (node-children-indexing-mixin place child)
  (:documentation "sets a node child depending on the implementation"))

(defmethod set-child ((node hash-node-children-indexing-mixin) place child)
  (setf (gethash place (children node)) child)
  (setf (parent child) node))

(defmethod add-child ((node hash-node-children-indexing-mixin) child)
  (set-child node child child))

(defun copy-tree-from-node (node)
  (parent-key-path-foldl node
		     (lambda (parent new-child child-key)
		       (set-child (copy-node parent) child-key new-child))
		     nil))

(defun replace-node (node new-node)
  (parent-key-path-foldl (parent node)
		     (lambda (parent new-child child-key)
		       (set-child (copy-node parent) child-key new-child))
		     new-node))

(defun child-key (child parent)
  (if (eql (gethash child (children parent)) child)
      child
      (maphash (lambda (k v) (when (eql v child) (return-from child-key k))) (children parent)))
  (error "Child not found ~A in ~A" child parent))

(defmethod print-tree ((node value-node-mixin) &optional (stream *standard-output*))
  (format stream "[~A " (value node))
  ; Important: we assume the node indexing comes after the node type in the hierarchy. The following (call-next-method) is supposed to print the children based on the indexing scheme
  (call-next-method))

(defmethod print-tree ((node hash-node-children-indexing-mixin) &optional (stream *standard-output*))
  (loop for (key . children) in (list-children node)
       do (progn
	    (format stream "~A -> " key)
	    (format stream "~A" (print-tree children stream))))
  (format stream "]"))

(defmethod list-children ((node hash-node-children-indexing-mixin))
  (let
      ((list '()))
    (maphash (lambda (k v) (push (cons k v) list)) (children node))
    list))


;;; (defmethod print-object ((node node-mixin) stream)
;;;   (print-unreadable-object (node stream :type t :identity t)
;;;     (format stream "parent: ~A" (parent node))))

(defmethod print-object ((node value-node-mixin) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "value: ~A" (value node))))

;; Example:

(defclass my-node (value-node-mixin hash-node-children-indexing-mixin)
  ()
  )



(defparameter *cnode* (make-instance 'my-node :value "copia"))

(defparameter *tree* (make-instance 'my-node :value 32
				    :children `((a . ,(make-instance 'my-node :value 22))
						     (b . ,(make-instance 'my-node :value 13 :children (list (cons :c *cnode*)))))))

(print-tree *tree*)


;; Error: no es posible hacer fat-copy con sentido ya que el puntero al nodo padre hace necesario copiar todo el arbol !!!
;; De todas formas, podemos implementar una búsqueda del nodo padre en lugar de actualizar punteros, aunque esto es costoso en tiempo (orden de ejecución O(n))


(defvar *tree* nil "The tree we are processing")

(defmethod parent ((node node-mixin))
  (node-parent node *tree*))

(defun node-parent (node tree)
  (search-node-parent node tree))

(defun search-node-parent (node tree &optional parent)
  (if (equal node tree)
      parent
      (loop for child in (children tree)
	   do (let ((it (search-node-parent node child tree)))
		(when it
		  (return-from search-node-parent it))))))

(copy-tree-from-node *cnode*)