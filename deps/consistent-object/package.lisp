(defpackage consistent-object
  (:nicknames :co)
  (:use :cl :closer-mop)
  (:export
   #:consistent-object-class
   #:consistent-object
   #:consistency-check
   #:consistency-error
   #:validate
   #:ensure
   #:suspending-consistency-for))

(defpackage consistent-object.test
  (:nicknames :co.test)
  (:use :cl :consistent-object :fiveam)
  (:export
   #:run-tests))