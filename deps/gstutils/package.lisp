(defpackage :gstutils
  (:use :cl :anaphora :sb-mop :log5 :sb-ext)
  (:shadow :name)
  (:import-from :arnesi :with-unique-names :let1)
  (:export
   #:defalias
   #:make-weakref #:make-wref #:mk-wref
   #:weakref-value #:wref-value
   #:list-lambda-list-vars
   #:replace-freevars
   #:list-free-vars
   #:list-free-vars-non-external
   #:prune-externals
   #:replace-all
   #:xmlize
   #:always-with
   
   ;; required-slots-class
   #:required-slots-class
   #:required-slot-error

   ;; make-lib
   #:make-let
   #:make-using
   #:make-with

   ;; references
   #:with-refs
   #:ref
   #:refp))

(defpackage gstutils.test
  (:use :cl :gstutils :fiveam))
