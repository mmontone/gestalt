(require :asdf)

(defpackage gstutils-system
  (:use :cl :asdf))

(in-package :gstutils-system)

(defsystem gstutils
    :name "gstutils"
    :version "0.0.1"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :licence "LLGPL"
    :description "A Common Lisp dataflow extension"
    :components
  ((:file "package")
   (:file "gstutils")
   (:file "copy-obj")
   )
  :depends-on (:hunchentoot)
  )