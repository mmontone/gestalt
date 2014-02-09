(defpackage :gestalt
  (:nicknames :gst)
  (:use
   :cl
   :anaphora
   ;:closer-mop
   ))

(defpackage :gestalt.dispatch
  (:nicknames :gst.dispatch)
  (:use :gestalt))

(defpackage :gestalt.user
  (:nicknames :gst.user)
  (:use :cl :gst :log5))

(defpackage :gestalt.test
  (:nicknames :gst.test)
  (:use :cl :gst :fiveam))
