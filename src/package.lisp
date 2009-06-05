(defpackage :gestalt
  (:nicknames :gst)
  (:use
   :cl
   :anaphora
   :gstutils
   ;:rucksack
   ;:dataflow
   ))

(defpackage :gestalt.dispatch
  (:nicknames :gst.dispatch)
  (:use :gestalt))

(defpackage :gestalt.user
  (:nicknames :gst.user)
  (:use :cl :gst :log5))
