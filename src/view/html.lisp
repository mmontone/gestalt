(in-package :gst.view.html)

(defclass html-element (xml-node)
  ()
  (:documentation "HTML nodes"))

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

(defclass div (xml-container html-element)
  ()
  (:documentation "An HTML div"))

(defclass p (html-element)
  ()
  (:documentation "An HTML paragraph"))

(defclass input (html-element)
  ((type :accessor input-type :initform "" :initarg :type))
  (:documentation "An HTML input"))
