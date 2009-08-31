(defpackage :gst.util
  (:use :cl :anaphora :sb-mop :log5 :alexandria)
  (:shadow :name)
  (:import-from :contextl :singleton-class :find-singleton)
  (:export
   #:defalias
   #:list-lambda-list-vars
   #:replace-freevars
   #:list-free-vars
   #:list-free-vars-non-external
   #:prune-externals
   #:replace-all
   #:always-with
   #:one-of
   #:foldl
   
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

(defpackage dlist
  (:use :cl)
  (:export #:dlist
	   #:make-dlist
	   #:dlist-head
	   #:dlist-tail
	   #:insert-between
	   #:insert-before
	   #:insert-after
	   #:insert-head
	   #:insert-tail
	   #:remove-link
	   #:dlist-elements
	   #:do-dlist
	   #:map-dlist
	   #:reduce-dlist
	   #:length-dlist
	   #:null-dlist
	   #:dlist-position))

(defpackage :gst.encode
  (:use :cl :cl-base64 :babel)
  (:export #:register-key
	   #:compress
	   #:inflate
	   #:encrypt
	   #:decrypt
	   #:string-to-octets
	   #:octets-to-string
	   #:usb8-array-to-base64-string
	   #:base64-string-to-usb8-array))

(defpackage :dlist.test
  (:use :cl :dlist :fiveam)
  (:export #:run-tests))

(defpackage gst.util.test
  (:use :cl :gst.util :fiveam)
  (:export #:run-tests))