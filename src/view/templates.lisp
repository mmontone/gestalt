(in-package :gst.view.templates)

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

(defclass container (html-element)
  ((id :accessor id
       :initform ""))
  (:documentation "Special tag. Embeds the component named by the id attribute"))

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

