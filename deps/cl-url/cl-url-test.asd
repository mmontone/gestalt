(defpackage cl-url-test-system
  (:use :cl :asdf))

(in-package :cl-url-test-system)

(defsystem cl-url-test
    :description "cl-url: A Common Lisp library for handling URLs"
    :version "0.1"
    :author "Mariano Montone <mariano@copyleft.no>"
    :maintainer "Mariano Montone <mariano@copyleft.no>"
    :licence "Public Domain"
    :components
    ((:file "test"))
    :serial t
    :depends-on
    (:cl-url :fiveam))