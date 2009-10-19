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
	       (:file "start"))
	      :serial t))
    :serial t
    :depends-on
    (:anaphora
     :alexandria
     :hunchentoot
     :log5
     :fiveam
     :cl-who
     :parenscript
     :css-lite
     :cl-cont
     :contextl
     :gst.util
     :dataflow
     :build-tree
     :code-walker
     :elephant))