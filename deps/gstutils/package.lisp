(defpackage :gstutils
  (:use :cl)
  (:export
   #:defalias
   #:make-weakref #:make-wref #:mk-wref
   #:weakref-value #:wref-value
   #:list-lambda-list-vars
   #:replace-freevars
   #:list-free-vars
   #:list-free-vars-non-external
   #:replace-all
   #:xmlize
   #:always-with))