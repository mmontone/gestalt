(defpackage :gstutils
  (:use :cl :anaphora :sb-mop :log5 :alexandria)
  (:shadow :name)
  (:import-from :arnesi :with-unique-names :let1)
  (:import-from :contextl :singleton-class :find-singleton)
  (:export
   #:defalias
   #:list-lambda-list-vars
   #:replace-freevars
   #:list-free-vars
   #:list-free-vars-non-external
   #:prune-externals
   #:replace-all
   #:xmlize
   #:always-with
   #:lisp-object-address
   #:one-of
   
   ;; required-slots-class
   #:required-slots-class
   #:required-slot-error
   
   ;; make-lib
   #:make-let
   #:make-using
   #:make-with
   #:make-as

   ;; references
   #:with-refs
   #:let-refs
   #:using-refs
   #:value
   #:ref
   #:refp

   ;; copy-object
   #:copy
   #:shallow-copy
   #:deep-copy))
  

(defpackage gstutils.test
  (:use :cl :gstutils :fiveam))