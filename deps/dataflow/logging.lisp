(in-package :dataflow)

;; log5 config
(defcategory mop)
(defcategory df)
(defcategory all
    (or mop df))
(defcategory none)

(defun start-df-tracking (&key (name 'df)
			  (stream *error-output*))
  (start-sender name
		(stream-sender :location stream)
		:category-spec (list name)
		:output-spec '(message)))

(defun stop-df-tracking (&optional (name 'df))
  (stop-sender name))

(defvar *debug* t "Turn off to disable debugging macros. Note you'll have to recompile your code in order for this to be effective")

(defmacro defdbgmacro (name args &rest body)
  `(defmacro ,name ,args
     (when *debug*
	 ,@body)))

(defdbgmacro dbg (&body body)
  `(progn
     ,@body))