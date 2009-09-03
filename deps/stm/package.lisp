(defpackage stm
  (:use :common-lisp
	:gst.util
	:sb-mop
	:log5
	:alexandria)
  (:export
   #:stm
   #:with-stm-vars
   #:let-stm-vars
   #:using-stm-vars
   #:atomically
   #:begin-stm-transaction
   #:rollback-stm-transaction
   #:commit-stm-transaction
   #:defunstm
   #:value
   #:with-bindings-as-stm-vars
   #:no-transaction-error
   #:retry-transaction
   #:abort-transaction
   #:continue
   ))

(defpackage stm.test
  (:use :cl
	:stm
	:gst.util
	:fiveam)
  (:export #:run-tests))