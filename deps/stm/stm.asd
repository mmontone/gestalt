(require :asdf)

(defpackage :stm-system
  (:nicknames :stmsys)
  (:use :cl :asdf))

(in-package :stm-system)

(defsystem stm
    :name "stm"
    :version "0.0.1"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :licence "LLGPL"
    :description "Simple transactional memory for Common Lisp"
    :components
    ((:file "package")
     (:file "logging")
     (:file "stm")
     (:file "test")
     )
    :serial t
    :depends-on (:gst.util
		 :log5
		 :code-walker))