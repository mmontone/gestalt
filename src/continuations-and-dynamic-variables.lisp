(require :cl-cont)

(use-package :cl-cont)

(defvar *my-dyn-var*)

(defvar *cont*)

(with-call/cc
  (let
      ((*my-dyn-var* "hello!!"))
    (call/cc (lambda (cont)
	       (setf *cont* cont)))
    (print *my-dyn-var*)))

(funcall *cont*)