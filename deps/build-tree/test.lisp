(in-package :build-tree)

;; This works for this:

#|


(tree (:html (:body :on-load "on load code" (:h1))))

(let ((x "hello"))
      (tree (:h1 x)))

(tree (:h1 (let ((x "hello")) x)))

;; But what happens if we want this:??
(tree (:html (esc (tree (:body :on-load "on load code" (:h1))))))

;; We can do this with chdn tag

(tree (:html (chdn (list (tree (:body :on-load "on load code" (:h1)))))))

;; We should implement a chdn1 tag for (chdn (list patterns ;)

(tree (:html (chd (tree (:body :on-load "on load code" (:h1))))))

;; We could use special tags:
;; The special tag children appends the list of children

(let ((children (loop for name in (list "hola" "chau" "adios")
		     collect (tree (:h1 name)))))
  (tree (:body (chdn children))))

|#