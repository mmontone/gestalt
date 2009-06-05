;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10.; Package: UT -*-
;;;_____________________________________________________________________________
;;;
;;;                       System: Common Lisp Utilities
;;;                       Module: copy objects
;;;                       Version: 1.0
;;;
;;; Copyright (c): Forschungsgruppe DRUID, Juergen Herczeg
;;;                Universitaet Stuttgart
;;;
;;; File: /usr/local/lisp/xit/cl-utilities/copy-objects.lisp
;;; File Creation Date: 03/19/92 13:58:02
;;; Last Modification Time: 09/18/92 14:33:57
;;; Last Modification By: Juergen Herczeg
;;;
;;;
;;; Changes (worth to be mentioned):
;;; ================================
;;;
;;;_____________________________________________________________________________

(in-package :gstutils)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(instance-slot-names
	    make-uninitialized-instance
	    uninitialized-copy
	    copy
	    shallow-copy
	    deep-copy)))

#-allegro
(eval-when (compile load eval)
  (export '(instance-slot-names
	    make-uninitialized-instance
	    uninitialized-copy
	    copy
	    shallow-copy
	    deep-copy)))

(defmethod instance-slot-names ((object standard-object))
  (let* ((slots (sb-mop:class-slots (class-of object)))
	 (instance-slots (remove :class slots
				 :key #'sb-mop:slot-definition-allocation
				 :test #'eq)))
    (mapcar #'sb-mop:slot-definition-name instance-slots)))

(defmethod make-uninitialized-instance ((class symbol) &rest initargs)
  (apply #'make-uninitialized-instance (find-class class) initargs))

(defmethod make-uninitialized-instance ((class standard-class)
					&rest initargs)
  (declare (ignore initargs))
  (allocate-instance class))

(defmethod shallow-copy (object &key &allow-other-keys)
  ;; primitive objects are not copied
  object)

(defmethod deep-copy (object &key &allow-other-keys)
  ;; primitive objects are not copied
  object)

(defmethod copy (object &key &allow-other-keys)
  (deep-copy object))

#-lucid
;; class metobject is not supplied by lucid clos
(defmethod uninitialized-copy ((object sb-mop:metaobject) &key &allow-other-keys)
  ;; metaobjects are not copied
  object)

(defmethod shallow-copy ((object sequence) &key &allow-other-keys)
  (copy-seq object))

(defmethod deep-copy ((object list) &key &allow-other-keys)
  (map 'list #'copy object))

(defmethod copy ((object sequence) &key &allow-other-keys)
  ;; sequences are not deep copied by default
  (shallow-copy object))

(defmethod slots-for-identity ((object standard-object))
  ())

(defmethod slots-for-shallow-copy ((object standard-object))
  ())

(defmethod slots-for-deep-copy ((object standard-object))
  ())

(defmethod slots-for-copy ((object standard-object))
  (instance-slot-names object))

(defmethod copy-slot ((from-object standard-object)
		      (to-object standard-object)
		      slot &optional (copy-function #'copy))
  (when (slot-boundp from-object slot)
    (setf (slot-value to-object slot)
	(funcall copy-function (slot-value from-object slot)))))

(defun delete-all (objects from)
  (loop for object in objects
       do (setf from (delete object from)))
  from)

(defmethod copy-slots ((from-object standard-object)
		       (to-object standard-object)
		       &key discard-slots)
  (dolist (slot (delete-all discard-slots
			    (slots-for-identity from-object)))
      (copy-slot from-object to-object slot #'identity))
  (dolist (slot (delete-all discard-slots
			    (slots-for-copy from-object)))
      (copy-slot from-object to-object slot #'copy))
  (dolist (slot (delete-all discard-slots
			    (slots-for-shallow-copy from-object)))
      (copy-slot from-object to-object slot #'shallow-copy))
  (dolist (slot (delete-all discard-slots
			    (slots-for-deep-copy from-object)))
      (copy-slot from-object to-object slot #'deep-copy)))

(defmethod object-creation-function ((object standard-object))
  #'make-uninitialized-instance)

(defmethod after-initialization ((old-object standard-object)
				 (new-object standard-object))
  nil)

(defmethod before-initialization ((old-object standard-object)
				  (new-object standard-object))
  nil)

(defmethod copy :around ((object standard-object)
			 &key discard-slots &allow-other-keys)
  (declare (special *copy-reference-list*))
  (if (boundp '*copy-reference-list*)
      (or (cdr (assoc object *copy-reference-list* :test #'eq))
	  (let ((copy (uninitialized-copy object)))
	    (unless (eq object copy)
	      (setq *copy-reference-list*
		  (acons object copy *copy-reference-list*))
	      (call-next-method object :use-object copy
				       :discard-slots discard-slots))
	    copy))
    (let ((*copy-reference-list* nil))
      (declare (special *copy-reference-list*))
      (let ((copy (uninitialized-copy object)))
	(unless (eq object copy)
	  (setq *copy-reference-list*
	      (acons object copy *copy-reference-list*))
	  (call-next-method object :use-object copy
				   :discard-slots discard-slots))
	copy))))

(defmethod uninitialized-copy ((object standard-object) &key &allow-other-keys)
  (let ((class-name (class-name (class-of object)))
	(creation-function (object-creation-function object)))
    (funcall creation-function class-name)))
	   
(defmethod deep-copy ((object standard-object) &key discard-slots use-object)
  (let ((new-object (or use-object (uninitialized-copy object))))
    (unless (eq new-object object)
      (before-initialization object new-object)
      (copy-slots object new-object :discard-slots discard-slots)
      (after-initialization object new-object))
    new-object))
	   
(defmethod shallow-copy ((object standard-object)
			 &key discard-slots use-object)
  (let ((new-object (or use-object (uninitialized-copy object))))
    (unless (eq new-object object)
      (dolist (slot (delete-all discard-slots (instance-slot-names object)))
	(copy-slot object new-object slot #'identity)))
    new-object))
	   
(defmethod copy ((object standard-object) &key discard-slots use-object)
  (deep-copy object :discard-slots discard-slots
	            :use-object use-object))

(defmethod get-copy (object)
  (declare (special *copy-reference-list*))
  (and (boundp '*copy-reference-list*)
       (cdr (assoc object *copy-reference-list* :test #'eq))))

;; Test:

#|

(defclass person ()
  ((name :initarg :name :initform (error "Provide the name"))
   (lastname :initarg :lastname :initform (error "Provide the lastname"))
   (friends :initform '() :documentation "His friends (type person)")
   (friend :documentation "A direct friend"))
  (:documentation "A person with friends so can experiment with copying"))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type t :identity t)
    (format stream "name: ~A lastname: ~A"
	    (slot-value person 'name)
	    (slot-value person 'lastname))))

(defparameter *mariano* (make-instance 'person :name "Mariano" :lastname "Montone"))

(defparameter *pedro* (make-instance 'person :name "Pedro" :lastname "Montone"))

(copy *mariano*)
(shallow-copy *mariano*)

(defun add-friend (person friend)
  (symbol-macrolet
      ((friends (slot-value person 'friends)))
    (setf friends (push friend friends))))

(add-friend *mariano* *pedro*)

(setf (slot-value *mariano* 'friend) *pedro*)
(setf (slot-value *pedro* 'friend) *mariano*) ;; Circular dependency

;; Now we copy, and it works!! (handles circularity successfully)
(deep-copy *mariano*)

;; It doesn't deep copy lists though (person friends are not copied), although it should not be too difficult to implement

|#