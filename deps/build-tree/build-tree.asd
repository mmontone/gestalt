(require :asdf)

(defpackage :btr-sys
  (:use :cl :asdf))

(in-package :btr-sys)

(defsystem build-tree
    :name "build-tree"
    :version "0.0.1"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :licence "LLGPL"
    :description "Tree building syntax"
    :components
    ((:file "package")
     (:file "build-tree")
     (:file "test"))
    :serial t)