(in-package :stm)

(defvar *debug* t "Turn off to disable debugging macros. Note you'll have to recompile your code in order for this to be effective")

(defmacro defdbgmacro (name args &rest body)
  `(defmacro ,name ,args
     (when *debug*
	 ,@body)))

(defdbgmacro trk (datum &rest args)
  `(when *track*
     (format t ,datum ,@args)))

(defdbgmacro dbg (&body body)
  `(progn
     ,@body))

(defcategory stm)
(defcategory none)

#|
(start-sender 'stm
  (stream-sender :location *error-output*)  
  :category-spec '(stm)
  :output-spec '(message))

(start-sender 'none
  (stream-sender :location *error-output*)  
  :category-spec '(none)
  :output-spec '(message))

|#