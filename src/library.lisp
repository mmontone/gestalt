;; First-class libraries.

;; Libraries are first class objects in the framework. They should have a unique name. We don't implement dependencies between them yet.
;; You can declare component library dependencies per component

(defvar *libraries* (make-hash-table :test #'equalp) "Defined libraries mapped by name")


(defclass library ()
  ((url :accessor url :initform (error "Provide the library URL") :documentation "The URL to access the library")
   (dependencies :accessor dependencies :initform '() :documentation "Others libraries this depends on")
   ))

(defclass library-class (standard-class)
  ()
  )

(defmacro deflibrary (name &rest options)
  `(defclass ,name ()
     ,@options
     (:metaclass 'library-class)))

(defmethod shared-initialize ((library library) &rest initargs)
  (declare (ignore initargs))
  ;; Register the defined library
  (setf (class-name ))
  
  )


;; Example:

(deflibrary prototype
    :url "/lib/prototype.js")

(deflibrary scriptaculous
    :url "/lib/scriptaculous.js"
    :dependencies '(prototype))

;; And then we can use it in our components

;; scriptaculous-component mixin
(defcomponent-mixin scriptaculous-component-mixin ()
  (:libraries '(scriptaculous)))

;; defcomponentmixin should create a non instanciable class and append its libraries to its subclasses
;; so we can do

(defcomponent my-scriptaculous-component (scriptaculous-component-mixin)
  ...
  )

;; Besides, we should get rid of circularities and repetition when building the main page. The main page required scripts are determined from the components handling repetition and circularity.