(defpackage :gst.view.xml
  (:use :cl
	:dlist
	:alexandria
	:gst.util
	:cl-who
	:json
	:parenscript
	:closer-mop)
  (:shadowing-import-from :alexandria #:switch)
  (:shadowing-import-from :json #:with-object)
  (:shadowing-import-from :closer-mop
			  #:defmethod
			  #:defgeneric
			  #:standard-generic-function)
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
	   #:make-base-tree
	   #:make-base-tree-node
	   #:base-tree-member-p
	   #:copy-xml-tree
	   ;; querying
	   #:childp
	   #:is-next
	   #:comes-after
	   #:is-previous
	   #:comes-before
	   #:do-children
	   #:collect-children
	   #:xml-node-equal
	   #:xml-tree-equal
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
	   #:target
	   #:child
	   #:replacement
	   #:reference-child
	   #:place
	   ;; modifications serialization
	   #:serialize-modifications
	   #:define-serialization-output))

(defpackage :gst.view.dom
  (:use :cl
	:gst.view.xml
	:gst.util
	:dlist)
  (:export #:dom-xml-node
	   #:get-node-with-id
	   #:*assign-ids*))

(defpackage :gst.view.html
  (:use :cl
	:gst.view.xml)
  (:export #:a
	   #:p
	   #:div
	   #:href
	   #:input
	   #:type
	   #:input-type
	   #:content))

(defpackage :gst.view
  (:use :cl
	:gst.view.xml
	:gst.view.html
	:gst.view.dom
	:gst.util
	:gst.encode)
  (:shadow #:a
	   #:div
	   #:input
	   #:p)
  (:export #:view-node
	   #:view-container
	   #:handler
	   #:controller
	   #:encoded-node-id
	   #:apply-modifications
	   #:apply-modification
	   ))

(defpackage :gst.view.templates
  (:use :cl
	:gst.view.xml
	:gst.view.html
	:closer-mop
	:alexandria)
  (:shadowing-import-from :xml #:container)
  (:shadowing-import-from :closer-mop
			  #:defmethod
			  #:defgeneric
			  #:standard-generic-function)
  (:export #:template
	   #:container))

(defpackage :gst.view.test
  (:use :cl
	:gst.view.xml
	:gst.view.dom
	:gst.view
	:gst.view.templates
	:5am)
  (:export #:run-tests))