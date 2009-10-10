(defpackage consistent-object-system
  (:use :cl :asdf))

(in-package :consistent-object-system)

(defsystem consistent-object
    :description "consistent-object: A Common Lisp extension for handling objects consistently"
    :version "0.1"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :licence "Public Domain"
    :components
    ((:file "package")
     (:file "consistent-object")
     (:file "test"))
    :serial t
    :depends-on
    (:closer-mop
     :fiveam))