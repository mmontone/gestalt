(require :asdf)

(defpackage gestalt-system
  (:use :cl :asdf))


(in-package :gestalt-system)

(defsystem gestalt
    :description "The Gestalt CL framework"
    :version "0.1"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :licence "Public Domain"
    :components ((:file "package")
		 (:file "start"))
    :depends-on (:hunchentoot))