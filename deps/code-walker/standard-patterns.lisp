(in-package :code-walker)

#|

block      let*                  return-from
catch      load-time-value       setq
eval-when  locally               symbol-macrolet
flet       macrolet              tagbody
function   multiple-value-call   the
go         multiple-value-prog1  throw
if         progn                 unwind-protect
labels     progv
let        quote

|#

(defun funcall-discarding (function arg)
  (declare (ignore arg))
  (funcall function))

(defun discard-arg (arg)
  (declare (ignore arg))
  nil)

(define-code-walker-pattern (lambda args &body body)
    :spec-matching-function (lambda (spec)
			      (and (listp spec)
				   (equalp (car spec) 'lambda)))
    :form-matching-function (lambda (form lexenv)
			      (declare (ignore lexenv))
			      (and (listp form)
				   (equalp (car form) 'lambda)))
    :inject-function (lambda (where form)
		       (destructuring-bind (lambda args &rest body) form
			 (declare (ignore lambda))
			 (funcall where args body)))
    :extract-function (lambda (spec)
			(destructuring-bind (lambda args rest-keyword body) spec
			  (declare (ignore lambda rest-keyword))
			  (list args body)))
    :doc "Matches a lambda form")

(define-code-walker-pattern (let bindings &rest body)
               :spec-matching-function (lambda (spec)
					 (and (listp spec)
					      (equalp (car spec) 'let)))
	       :form-matching-function (lambda (form lexenv)
					 (declare (ignore lexenv))
					 (and (listp form)
					      (equalp (car form) 'let)))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (let bindings &rest body) form
	       			    (declare (ignore let))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (spec)
				   (destructuring-bind (let bindings rest-keyword body) spec
				     (declare (ignore let rest-keyword))
				     (list bindings body)))
	       :doc "Matches a let form")

(define-code-walker-pattern (let* bindings &rest body)
               :spec-matching-function (lambda (spec)
					 (and (listp spec)
					      (equalp (car spec) 'let*)))
	       :form-matching-function (lambda (form lexenv)
					 (declare (ignore lexenv))
					 (and (listp form)
					      (equalp (car form) 'let*)))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (let* bindings &rest body) form
	       			    (declare (ignore let*))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (pattern)
				   (destructuring-bind (let* bindings rest-keyword body) pattern
				     (declare (ignore let* rest-keyword))
				     (list bindings body)))
	       :doc "Matches a let* form")

(define-code-walker-pattern (flet bindings &rest body)
               :spec-matching-function (lambda (spec)
					 (and (listp spec)
					      (equalp (car spec) 'flet)))
	       :form-matching-function (lambda (form lexenv)
					 (declare (ignore lexenv))
					 (and (listp form)
					      (equalp (car form) 'flet)))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (flet bindings &rest body) form
	       			    (declare (ignore flet))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (pattern)
				   (destructuring-bind (flet bindings rest-keyword body) pattern
				     (declare (ignore flet rest-keyword))
				     (list bindings body)))
	       :doc "Matches an flet form")

(define-code-walker-pattern (labels bindings &rest body)
               :spec-matching-function (lambda (spec)
					 (and (listp spec)
					      (equalp (car spec) 'labels)))
	       :form-matching-function (lambda (form lexenv)
					 (declare (ignore lexenv))
					 (and (listp form)
					      (equalp (car form) 'labels)))
	       :inject-function (lambda (where form)
	       			  (destructuring-bind (labels bindings &rest body) form
	       			    (declare (ignore labels))
	       			    (funcall where bindings body)))
	       :extract-function (lambda (pattern)
				   (destructuring-bind (labels bindings rest-keyword body) pattern
				     (declare (ignore labels rest-keyword))
				     (list bindings body)))
	       :doc "Matches an labels form")

(define-code-walker-pattern *special-variable*
               :spec-matching-function (lambda (spec) (equalp spec '*special-variable*))
	       :form-matching-function (lambda (form lexenv)
					 (declare (ignore lexenv))
					 (and (symbolp form)
					      (dynamic-variable-symbol-p form)))
	       :inject-function #'funcall-discarding
	       :extract-function #'discard-arg
	       :doc "Matches a special variable")

(define-code-walker-pattern (quote form)
  :spec-matching-function (lambda (spec)
			    (and (listp spec)
				 (equalp (car spec) 'quote)))
  :form-matching-function (lambda (form lexenv)
			    (declare (ignore lexenv))
			    (and (listp form)
				 (equalp (car form) 'quote)))
  :inject-function (lambda (where form) (funcall where (cadr form)))
  :extract-function #'cdr
  :doc "Matches a quoting form")

(define-code-walker-pattern (return-from form &optional value)
    :spec-matching-function (lambda (spec)
			      (and (listp spec)
				   (equalp (car spec) 'return-from)))
    :form-matching-function (lambda (form lexenv)
			      (declare (ignore lexenv))
			      (and (listp form)
				   (equalp (car form) 'return-from)))
    :inject-function (lambda (where form)
		       (funcall where (cadr form)))
    :extract-function (lambda (form)
			(destructuring-bind (return-from tag &optional optional-keyword value) form
			  (declare (ignore return-from optional-keyword))
			  (list tag value)))
    :doc "Matches a return-from form")

(define-code-walker-pattern (declare declaration-exp)
    :spec-matching-function (lambda (spec)
			      (and (listp spec)
				   (equalp (car spec) 'declare)))
    :form-matching-function (lambda (form lexenv)
			      (declare (ignore lexenv))
			      (and (listp form)
				   (equalp (car form) 'declare)))
    :inject-function (lambda (where form)
		       (funcall where (cadr form)))
    :extract-function #'cdr
    :doc "Matches a declare form")

(define-code-walker-pattern :keyword
    :spec-matching-function (lambda (spec)
			      (equalp spec :keyword))
    :form-matching-function (lambda (form lexenv)
			      (declare (ignore lexenv))
			      (keywordp form))
    :inject-function #'funcall-discarding
    :extract-function #'discard-arg)

(define-code-walker-pattern :number
    :spec-matching-function (lambda (spec)
			      (equalp spec :number))
    :form-matching-function (lambda (form lexenv)
			      (declare (ignore lexenv))
			      (numberp form))
    :inject-function #'funcall-discarding
    :extract-function #'discard-arg)

(define-code-walker-pattern :lexvar
    :spec-matching-function (lambda (spec)
			      (equalp spec :lexvar))
    :form-matching-function (lambda (form lexenv)
			      (and (symbolp form)
				   (member form lexenv)))
    :inject-function #'funcall-discarding
    :extract-function #'discard-arg)

(define-code-walker-pattern :freevar
    :spec-matching-function (lambda (spec) (equalp spec :freevar))
    :form-matching-function (lambda (form lexenv)
			      (and (symbolp form)
				   (not (keywordp form))
				   (not (dynamic-variable-symbol-p form))
				   (not (member form lexenv))))
    :inject-function #'funcall-discarding
    :extract-function #'discard-arg)

(define-code-walker-pattern (f args)
    :spec-matching-function (lambda (spec) (equalp spec '(f args)))
    :form-matching-function (lambda (form lexenv)
			      (declare (ignore lexenv))
			      (and (listp form)
				   (not (null form))))
    :inject-function (lambda (where form)
		       (funcall where (car form) (cdr form)))
    :extract-function #'identity)