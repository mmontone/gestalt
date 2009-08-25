(defpackage :gst.view.xml
  (:use :cl
	:dlist
	:alexandria
	:gst.util
	;:xml
	:cl-who
	:json
	:parenscript
	:closer-mop)
  (:shadowing-import-from :alexandria #:switch)
  (:export #:xml-node
	   #:parent
	   #:children
	   #:node-id
	   #:xml-container
	   ;; node operations
	   #:append-child
	   #:replace-child
	   #:remove-child
	   #:insert-child-before
	   #:insert-child-after
	   #:insert-child
	   #:attribute
	   ;; modifications tracking
	   #:tracked-xml-node
	   #:*register-modifications*
	   #:make-base-tree
	   #:modified-p
	   #:flush-modifications
	   #:is-appended
	   #:is-inserted
	   #:is-a-replacement
	   #:extract-modifications
	   #:xml-node-modification
	   #:append-child-modification
	   #:insert-child-modification
	   #:replace-child-modification
	   #:remove-child-modification
	   #:append-modification-p
	   #:insert-modification-p
	   #:replace-modification-p
	   #:remove-modification-p
	   ;; modifications serialization
	   #:serialize-modifications
	   #:define-serialization-output))

(defpackage :gst.view.dom
  (:use :cl
	:gst.view.xml
	:gst.util
	;:xml
	)
  (:export #:dom-xml-node))


(defpackage :gst.view.html
  (:use :cl
	:gst.view.xml
	;:xml
	)
  (:export #:a
	   #:p
	   #:div))

(defpackage :gst.view.templates
  (:use :cl
	:gst.view.html
	;:xml
	)
  (:shadowing-import-from :xml #:container)
  (:export #:template
	   #:container))

(defpackage :gst.view
  (:use :cl
	:gst.view.xml
	:gst.view.dom
	:gst.view.templates
	:gst.util
	:gst.encode
	;:xml
	)
  (:export #:view-node))

(defpackage :gst.view.test
  (:use :cl
	:gst.view.xml
	:gst.view.dom
	:gst.view
	;:xml
	:gst.view.templates
	:5am))
