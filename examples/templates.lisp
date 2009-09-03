(progn
  (require :xmlisp)
  (require :alexandria)
  (require :closer-mop)
  (load #p"/home/marian/src/lisp/gestalt/examples/dlist.lisp"))

(defpackage :gst.templates
  (:use :cl
	:xml
	:dlist
	:closer-mop
	:alexandria)
  (:export #:template
	   #:style-sheets
	   #:style-sheet
	   #:libraries
	   #:library
	   #:a
	   #:tag-name))

(in-package :gst.templates)

(defclass xml-node (xml-serializer)
   ((node-id :initarg :id
	     :accessor node-id
	     :initform nil
	     :documentation "A string that uniquely identifies the node in the tree")
    (parent :initarg :parent
	    :accessor parent
	    :documentation "The node's parent")
    (parent-link :initarg :parent-link
		 :accessor parent-link
		 :documentation "The node's link in parent's children (the dlist's dlink)"))
   (:documentation "mother of all HTML element classes"))

(defmethod print-slot-with-name-p ((xml-node xml-node) name)
  (not (one-of ("parent-link" "parent") name
	       :test #'string-equal)))

(defmethod tag-name ((xml-node xml-node))
  (class-name (class-of xml-node)))

(defmethod map-object ((dlist dlist) function)
  (map-dlist function dlist))

(defmethod xml-printable-as-subelement-p ((dlist dlist))
  t)

(defclass xml-container (xml-node)
  ((children :accessor children
	     :initform (make-dlist)))
  (:documentation "An HTML element that contains other HTML elements. Use as a mixin"))

(defmethod append-child ((xml-container xml-container) child)
  (add-subobject xml-container child))

(defmethod add-subobject ((xml-container xml-container)
			  (html-element html-element))
  (let ((parent-link (insert-tail (children xml-container)
				  html-element)))
    (setf (parent-link html-element) parent-link)
    (setf (parent html-element) xml-container)))

(defclass template (xml-container html-element)
   ((component-class
     :initarg :component-class
     :accessor component-class
     ;:initform (error "Provide the component class")
     :initform ""
     :documentation "The class of components this template applies to")
    (description
     :initarg :description
     :accessor description
     :initform "This template has not description")
    (libraries
     :initarg :libraries
     :accessor libraries
     :initform nil
     :documentation "The libraries the template depends on")
    (style-sheets
     :initarg :style-sheets
     :accessor style-sheets
     :initform nil
     :documentation "The style-sheets the template depends on"))
  (:documentation "A Gestalt template"))

(defclass style-sheets (xml-serializer)
  ((style-sheets :accessor style-sheets
		 :initform nil))
  (:documentation "Template style-sheets"))

(defclass style-sheet (xml-serializer)
  ((url :accessor url
	:initform ""))
  (:documentation "A style-sheet"))

(defclass libraries (xml-serializer)
  ((libraries :accessor libraries
	      :initform nil))
  (:documentation "Template libraries"))

(defclass library (xml-serializer)
  ((name :accessor name
	 :initform ""))
  (:documentation "A library"))

(defmethod add-subobject ((libraries libraries)
			  (library library))
   (add-object-to-slot libraries library 'libraries))

(defclass html (xml-container html-element)
   ()
   (:documentation "Contains all the html items of an HTML document"))

(defclass a (html-element)
   ((href :accessor href :initform "" :initarg :href))
   (:documentation "HTML link"))
 
(defclass font (html-element)
   ((face :accessor face)
   (size :accessor size :type number))
   (:documentation "Font info"))

(defclass container (html-element)
  ((id :accessor id
       :initform ""))
  (:documentation "Special tag. Embeds the component named by the id attribute"))

(defclass div (xml-container html-element)
  ()
  (:documentation "An HTML div"))

(defclass p (html-element)
  ()
  (:documentation "An HTML paragraph"))

(defmethod map-object ((dlist dlist) function)
  (map-dlist function dlist))

(defmethod xml-printable-as-subelement-p ((dlist dlist))
  t)

#|

(swank:inspect-in-emacs
<template component-class="person-editor"
          description="A template for a person-editor">
   <libraries>
     <library name="prototype-library"/>
   </libraries>
   <style-sheets>
     <style-sheet url="http://www.my-style.com/style.css"/>
     <style-sheet url="http://www.my-style.com/style2.css"/>
   </style-sheets>
   <div>
     <p>Hola!!</p>
     <a href="http://www.agentsheets.com">AgentSheets</a>
     <a href="http://www.agentsheets2.com">AgentSheets</a>
     <container id="name"/>
     <container id="lastname"/>
     <container id="address"/>
     <template component-class="person-viewer">
        <p>Chaoooo!!</p>
     </template>
   </div>  
</template>)
|#

;; (defmethod initialize-instance :after ((template template) &rest initargs)
;;   (declare (ignore initargs))
;;   (when (null (component-class template))
;;     (error "Provide the component-class")))
  
