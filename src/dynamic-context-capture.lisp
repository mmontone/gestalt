(require :contextl)

(use-package :contextl)

;; First example:

(defvar *env*)

(defun foo () (capture-dynamic-environment))

(setq *env* (dynamic-wind
               (handler-case (proceed (foo))
                 (error () (format t "I caught an error.")))))

(with-dynamic-environment (*env*)
   (error "boo!"))


;; Second example:

(defvar *mark*)

(defun foo ()
   (dynamic-wind (print 'foo) (proceed (bar))))

(defun bar ()
   (with-dynamic-mark (*mark*) (baz)))

(defun baz ()
   (dynamic-wind (print 'baz) (proceed (bam))))

(defun bam ()
   (capture-dynamic-environment *mark*))

(setq *env* (foo))

(with-dynamic-environment (*env*)
   (print 'duh))

;; Dynamic binding chaining test

(defun foo ()
   (dynamic-wind (print 'foo) (proceed (bar))))

(defun bar ()
  (with-dynamic-environment (*env*) (baz)))

(defun baz ()
   (dynamic-wind (print 'baz) (proceed (bam))))

(defun bam ()
   (capture-dynamic-environment))

(setq *env* (foo))

(with-dynamic-environment (*env*)
   (print 'duh))

;; Another dynamic binding chaining test

(defun foo ()
   (dynamic-wind (print 'foo) (proceed (bar))))

(defun bar ()
  (capture-dynamic-environment))

(defun baz ()
   (dynamic-wind (print 'baz) (proceed (bam))))

(defun bam ()
   (capture-dynamic-environment))

(setq *env* (foo))

(with-dynamic-environment (*env*)
   (setq *env* (baz))
   (with-dynamic-environment (*env*)
     (print 'duh)))

;; Significant dynamic binding chaining test

(defun foo () (capture-dynamic-environment))

(define-condition a-condition ()
  ())

(define-condition b-condition ()
  ())

(defmethod compose-dynamic-environments
    ((former dynamic-environment)
     (newer dynamic-environment))
  "This function is supposed to compose two dynamic-environments. Is it too naive? Does it work correctly??"
  (make-instance 'dynamic-environment
		 :dynamic-winds (append (slot-value newer 'contextl::dynamic-winds)
					(slot-value former 'contextl::dynamic-winds))))

(setq *env*
      (dynamic-wind
	(handler-case (proceed (foo))
	  (a-condition () (format t "I caught an a-condition!")))))



(with-dynamic-environment (*env*)
   (error 'a-condition))

(with-dynamic-environment (*env*)
  (error 'b-condition))

;; Dynamic environment composition??
(with-dynamic-environment (*env*)
  (setq *env*
	(dynamic-wind
	  (handler-case (proceed (foo))
	    (b-condition () (format t "I caught a b-condition"))))))

(setq *env* (compose-dynamic-environments *env*
					  (dynamic-wind
					    (handler-case (proceed (foo))
					      (b-condition () (format t "I caught a b-condition"))))))



;; Now both the error and the warning should get caught by the corresponding handler:
(with-dynamic-environment (*env*)
   (error 'a-condition))

(with-dynamic-environment (*env*)
  (error 'b-condition))

;; Mmm..that didn't work. Let's see this:

(defun foo () (capture-dynamic-environment))

(defun cont ()
  (dynamic--wind
   (handler-case (proceed (foo))
	  (b-condition () (format t "I caught an b-condition!")))))

(setq *env*
      (dynamic-wind
	(handler-case (proceed (cont))
	  (a-condition () (format t "I caught an a-condition!")))))



(with-dynamic-environment (*env*)
  (error 'a-condition))

(with-dynamic-environment (*env*)
  (error 'b-condition))


;; Syntax sugar


(defdynamic value 42)

(defun foo () (print (dynamic value)))

(defun bar () (dynamic-let ((value 4711)) (foo)))

(foo)
(bar)

;; Dynamic binding recomputation:

(defdynamic value '(d e f))

(setq *env* (dynamic-relet ((value (append '(a b c) (dynamic value))))
               (capture-dynamic-environment)))

(with-dynamic-environment (*env*) (dynamic value))

(setf (dynamic value) '(4 5 6))

(with-dynamic-environment (*env*) (dynamic value))




;; Question to pascal regarding continuations and bindings:

 with-dynamic-mark can bind both lexical and special variables,
so it can be used both for lexically and dynamically scoped partial
continuations.

Isn't it true that continuation implementations based on code transformation already bind lexical variables correctly??

;; Some way of composing/augmenting dynamic environments??