(require :asdf)

(defpackage :dataflow-system
  (:nicknames :dfsys)
  (:use :cl :asdf))

(in-package :dataflow-system)

(defsystem dataflow
    :name "dataflow"
    :version "0.0.1"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :licence "LLGPL"
    :description "A Common Lisp dataflow extension"
    :components
    ((:file "package")
     (:file "logging")
     (:file "dataflow")
     (:file "mop")
     (:file "test"))
    :serial t
    :depends-on (:gst.util
		 :log5
		 :fiveam
		 :trivial-garbage))
