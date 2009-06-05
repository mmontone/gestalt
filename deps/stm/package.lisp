(defpackage stm
  (:use :common-lisp
	:gstutils
	:sb-ext
	:sb-mop
	:log5))

(defpackage stm.test
  (:use :cl :stm))