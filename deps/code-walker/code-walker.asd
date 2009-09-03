(require :asdf)

(defpackage :code-walker-system
  (:nicknames :cw-sys)
  (:use :cl :asdf))

(in-package :code-walker-system)

(defsystem code-walker
    :name "code-walker"
    :version "0.0.1"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :licence "LLGPL"
    :description "An extensible Common Lisp code walker"
    :components
    ((:file "package")
     (:file "code-walker")
     (:file "standard-patterns")
     (:file "standard-code-walker")
     ;(:file "test")
     )
    :serial t
    :depends-on (:gst.util
		 :log5
		 :fiveam
		 :arnesi
		 :anaphora))