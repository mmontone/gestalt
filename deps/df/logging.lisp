(in-package :df)

;; log5 config
(defcategory mop)
(defcategory df)
(defcategory all
    (or mop df))
(defcategory none)

#|
(start-sender 'mop  
  (stream-sender :location *error-output*)  
  :category-spec '(mop)
  :output-spec '(message))

(start-sender 'df  
  (stream-sender :location *error-output*)  
  :category-spec '(df)
  :output-spec '(message))

(start-sender 'all
  (stream-sender :location *error-output*)  
  :category-spec '(all)
  :output-spec '(message))

(start-sender 'none
  (stream-sender :location *error-output*)  
  :category-spec '(none)
  :output-spec '(message))



|#

(defvar *debug* t "Turn off to disable debugging macros. Note you'll have to recompile your code in order for this to be effective")

(defmacro defdbgmacro (name args &rest body)
  `(defmacro ,name ,args
     (when *debug*
	 ,@body)))

(defdbgmacro dbg (&body body)
  `(progn
     ,@body))