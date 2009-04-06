(require :asdf)

(defpackage gestalt-system
  (:use :cl :asdf))


(in-package :gestalt-system)

(defsystem gestalt
    :description "The Gestalt Common Lisp Web Framework"
    :version "0.1"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :licence "Public Domain"
    :components
    ((:module src
	      :components
	      ((:file "package")
	       (:file "conditions")
	       (:file "dispatcher")
	       (:file "start"))))
    :depends-on
    (:hunchentoot
     :log5
     :cl-who
     :parenscript
     :cl-cont
     :contextl
     :gstutils
     :dataflow
     :build-tree
     ))