(in-package :gestalt)

(defvar *libraries* (make-hash-table :test #'equalp)
  "javascript and css libraries")

(defvar *use-libraries* '()
  "javascript and css libraries to use")

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro use-library (name)
  `(%use-library ',name))

  (defmacro with-reset-libraries ((&optional (libraries '*use-libraries*)) &body body)
  `(let ((,libraries '()))
     ,@body))

  (defmacro define-library (name (&key depends-on)
			    &body spec)
    `(setf (gethash ',name *libraries*)
	   (list :name ',name
		 :depends-on ',depends-on
		 :render
		 (lambda ()
		   ,@spec)))))

(defun reset-libraries ()
  (setf *use-libraries* '()))

(defun find-library (name)
  (multiple-value-bind (library found-p)
      (gethash name *libraries*)
    (if (not found-p)
	(error "Library ~A not found" name)
	library)))

(defun %use-library (name)
  (let ((library (find-library name)))
    (loop for dependent-library in (getf library :depends-on)
	 do (%use-library dependent-library))
    (when (not (find name *use-libraries*
		     :test #'equalp
		     :key (lambda (library)
			    (getf library :name))))
      (setf *use-libraries*
	    (append *use-libraries* (list library))))))

(defun %use-libraries (&rest libs)
  (loop for lib in libs
     do (%use-library lib)))

(defmacro use-libraries (&rest libs)
  `(apply #'%use-libraries ',libs))

(defun include-libraries (&optional (libraries *use-libraries*))
  (loop for library in libraries
       do (funcall (getf library :render))))
