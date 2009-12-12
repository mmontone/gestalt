(progn
  (require :alexandria)
  (require :anaphora)
  (require :closer-mop)
  (require :trivial-garbage)
  (require :gst.util)
  (require :log5)
  )
(defpackage dataflow
  (:nicknames :df)
  (:use :common-lisp
	:gst.util
	:closer-mop
	:log5
	:trivial-garbage
	:alexandria
	:anaphora)
  (:shadow #:add-dependent #:remove-dependent)
  (:shadowing-import-from :cl
			  #:defmethod
			  #:defgeneric
			  #:standard-generic-function)
  (:export
   ; dataflow cell definitions
   #:cell
   #:dependents
   #:event-dependents
   #:value-cell
   #:value
   #:test
   #:standard-event
   #:event
   #:changed
   #:formula
   ; dependency management
   #:add-dependent
   #:remove-dependent
   ; event handling policies
   #:define-event-handling-policy
   #:with-events-handling-policy
   ; events propagation
   #:trigger-event
   ; mop
   #:dataflow-class
   ; dataflow syntax
   #:mk-formula
   #:mk-lambda
   #:let-df-vars
   #:using-df-vars
   #:with-df-vars
   #:as-df-vars
   #:with-df-slots
   ; logging
   #:start-df-tracking
   #:stop-df-tracking
   #:df
   #:all
   #:mop
   #:none
   ; conditions
   #:dependency-exists))

(defpackage dataflow.test
  (:nicknames :df.test)
  (:use :cl :dataflow :fiveam)
  (:shadowing-import-from :fiveam #:test)
  (:export #:run-tests))

(defpackage dataflow.examples
  (:nicknames :df.ex)
  (:use :cl :dataflow))