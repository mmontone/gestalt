(in-package :cl-url)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-url-protocol (name &optional (str `(symbol-name ',name)))
    `(setf (gethash ',name *url-protocols*) ,str)))

(defvar *url-protocols*
  (make-hash-table :test #'equal))

(defclass url ()
  ()
  (:documentation "A Uniform Resource Locator"))

(defclass generic-url (url)
  ((protocol :initarg :protocol
	     :accessor protocol
	     :initform nil)
   (hostname :initarg :hostname
	     :accessor hostname
	     :initform nil)
   (port :initarg :port
	 :accessor port
	 :initform nil
	 :documentation "The url port")
   (path :initarg :path
	 :accessor path
	 :initform '()
	 :documentation "A list of names forming a path")
   (program :initarg :program
	    :accessor program
	    :initform nil)
   (parameters :initarg :parameters
	       :accessor parameters
	       :initform '()
	       :documentation "Property list from ?a1=v1&a2=v2&...&an=vn")
   (fragment :initarg :fragment
	     :accessor fragment
	     :initform nil))
  (:documentation "A typical URL. Example: protocol://hostname:port/path/program?a1=v1&a2=v2&...&an=vn#fragment"))

(defmethod initialize-instance :after ((url generic-url) &rest initargs)
  (declare (ignore initargs))
  (validate-protocol (protocol url))
  (validate-url-parameters (parameters url)))

(defun make-url (&rest initargs)
  (apply #'make-instance 'generic-url initargs))

(defmethod (setf protocol) :before (new-value (url generic-url))
  (validate-protocol new-value))

(defmethod (setf parameters) :before (new-value (url generic-url))
  (validate-url-parameters new-value))

(defun protocol-name (protocol)
  (string-downcase (symbol-name protocol)))

;; ;; Taken from:
;; ;;
;; ;; Cluts web application framework
;; ;;
;; ;; Copyright (c) 2003 Tor Henrik Hanken, torhenrik@copyleft.no
;; ;; This code is under the GPL.
;; ;;

;; (defun try-decode-utf-8 (string)
;;   (debug-log "utf-8-decode ~S" string)
;;   (handler-case 
;;       (flexi-streams:octets-to-string
;;        (flexi-streams:string-to-octets
;; 	string :external-format :latin1) :external-format :utf-8)
;;     (flexi-streams:external-format-encoding-error () string)))

;; (defun url-unescape (string)
;;   "Unescape parameter-strings.  Taken from IMHO."
;;   (let ((dest (make-string (length string))))
;;     (do ((i 0 (incf i))
;;          (j 0 (incf j)))
;;         ((= i (length string)) (try-decode-utf-8 (subseq dest 0 j)))
;;       (cond ((equal (aref string i) #\%)
;;              (incf i)
;;              (setf (aref dest j)
;;                    (code-char
;;                     (parse-integer string :start i :end (+ i 2) :radix 16)))
;;              (incf i))
;;             ((equal (aref string i) #\+)
;;              (setf (aref dest j)
;;                    #\Space))
;;             (t
;;              (setf (aref dest j)
;;                    (aref string i)))))))


;; (defparameter *html-escaped*
;;   '((#\& . "&amp;")
;;     (#\< . "&lt;")
;;     (#\> . "&gt;")))

;; (defun url-escape (string)
;;   (with-output-to-string (s)
;;     (loop for char across string do
;; 	 (cond ((char= char #\Space)
;; 		(princ #\+ s))
;; 	       ((or (char<= #\0 char #\9)
;; 		    (char<= #\a char #\z)
;; 		    (char<= #\A char #\Z)
;; 		    (find char "-_.," :test #'char=))
;; 		(princ char s))
;; 	       (t
;; 		(format s "~{%~2,'0X~}"
;; 			(coerce (flex:string-to-octets (string char)
;; 						       :external-format :utf-8)
;; 				'list)))))))

;; (defun html-escape (str)
;;   "return HTML escaped string (eg, '<' replaced by '&lt;')"
;;   (dolist (pair *html-escaped*)
;;     (setq str (string-replace (car pair) (cdr pair) str)))
;;   str)

;; (defun xml-escape (str)
;;   (html-escape str))

;; (defun try-decode-utf8-parameters (parameters)
;;   (loop for parameter in parameters
;;        collect (if (consp (cdr parameter))
;; 		   (list (try-decode-utf-8 (first parameter))
;; 			 (second parameter)
;; 			 (try-decode-utf-8 (third parameter))
;; 			 (fourth parameter)) 
;; 		   (cons  (try-decode-utf-8 (car parameter))
;; 			  (try-decode-utf-8 (cdr parameter))))))

;; ;; end of stolen code


; standard-protocols

(define-url-protocol :http)
(define-url-protocol :ftp)

(defun validate-protocol (protocol)
  (when (null protocol)
    (return-from validate-protocol protocol))
  (maphash (lambda (registered-protocol name)
	     (declare (ignore name))
	     (when (equalp registered-protocol protocol)
	       (return-from validate-protocol protocol)))
	   *url-protocols*)
      (error "~A is not a valid URL protocol" protocol))

(defun validate-url-parameters (params)
  ;(when (not (alexandria:proper-list-p params))
  ;  (error "URL parameters: ~A should be a property list" params))
  params)

(defun url-string (url)
  (format nil "~@[~(~a~):~]~@[//~a~]~@[:~a~]/~@[~{~(~a~)/~}~]~@[~a~]~@[?~{~(~a~)=~a~^&~}~]~@[#~a~]"
	  (protocol url)
	  (hostname url)
	  (port url)
	  (path url)
	  (program url)
	  (parameters url)
	  (fragment url)))

(defmethod print-object ((url generic-url) stream)
  (format stream "#u~s" (url-string url)))

(defvar *default-url-defaults* (make-instance 'generic-url
					      :protocol :http
					      :hostname "localhost"))

(define-condition merging-error (simple-error)
  ()
  (:documentation "An error of this kind is raised when an error ocurs when merging urls"))

(defun merge-urls (url
		   &key
		   (defaults *default-url-defaults*)
		   (properties '(protocol
				 hostname
				 port
				 path
				 program
				 parameters
				 fragment))
		   (on-error nil on-error-p))
  ":on-error specifies the restart to call. One of :use-default :use-url or nil"
  (if on-error-p
      (handler-bind ((merging-error #'(lambda (error)
					(invoke-restart on-error))))
	(%merge-urls url
		     :defaults defaults
		     :properties properties))
      (%merge-urls url
		   :defaults defaults
		   :properties properties)))

(defun %merge-urls (url
		   &key
		   (defaults *default-url-defaults*)
		   (properties '(protocol
				 hostname
				 port
				 path
				 program
				 parameters
				 fragment)))
  (let ((new-url (make-instance 'generic-url)))
    (flet ((assign-default-property (prop)
	     (funcall (fdefinition `(setf ,prop))
		      (funcall (fdefinition prop) url)
		      new-url))
	   (assign-url-property (prop)
	     (funcall (fdefinition `(setf ,prop))
		      (funcall (fdefinition prop) defaults)
		      new-url)))
    (loop for prop in properties
       do (cond
	    ((null (funcall (fdefinition prop) url))
	     (assign-default-property prop))
	    ((null (funcall (fdefinition prop) defaults))
	     (assign-url-property prop))
	    (t (restart-case
		   (error 'merging-error)
		 (continue ()) ;we do nothing. the property does not get assigned
		 (use-value (value)
		   (funcall (fdefinition `(setf ,prop))
			    value
			    new-url))
		 (use-default ()
		   (assign-default-property prop))
		 (use-url ()
		   (assign-url-property prop))))))
    new-url)))

(defun copy-url (url)
  (merge-urls url (make-instance 'generic-url)))

 
(defun url-equal (url1 url2)
  (loop for prop in '(port hostname port path program parameters fragment)
       when (not (equalp 
		  (funcall (fdefinition prop) url1)
		  (funcall (fdefinition prop) url2)))
       do (return-from url-equal nil))
  t)
  

(defun extract-url (url properties)
  (merge-urls url
	      (make-instance 'generic-url)
	      properties))

(defun parse-url (str)
  (let ((regex "(?:(\\w+)\\:)?(?:\\/\\/([\\w|%|-|\\.|\\d]*))?(?:\\:(\\d+))?(\\/(?:[\\w|%|-|\\.|\\d]+\\/)+)?(\\/?[\\w|%|-|\\d|\\.]+)?(?:\\?([\\w|\\d|%|-]+=[\\w|\\d|%|-]*(?:&[\\w|\\d|%|-]+=[\\w|\\d|%|-]*)*))?(?:#([\\w|\\d|%|-]*))?"))
    (multiple-value-bind (result matches)
	(cl-ppcre:scan-to-strings regex str)
      (declare (ignore result))
      (apply #'values (coerce matches 'list)))))

(defun read-url (str)
  (multiple-value-bind (protocol
			hostname
			port
			path
			program
		        parameters
			fragment)
      (parse-url str)
    (apply #'make-instance 'generic-url
	   `(:protocol ,(when protocol
			      (intern (string-upcase protocol) :keyword))
	     :hostname ,(when (and (stringp hostname)
				  (plusp (length hostname)))
			 hostname)
	     :port ,(when port (parse-integer port))
	     :path ,(remove-if (lambda (p) (zerop (length p)))
			       (when path
				 (split-sequence:split-sequence #\/ path)))
	     :program ,(when program
			     (remove #\/ program))
	     :parameters ,(when parameters
				(loop for param-and-value in
				     (split-sequence:split-sequence #\& parameters)
				   appending (split-sequence:split-sequence #\= param-and-value) into result
				     finally (return result)))
	     :fragment ,fragment))))
      
;; Reader plug
(defun sharp-url (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((namestring (read stream t nil t)))
    (unless *read-suppress*
      (read-url namestring))))

(set-dispatch-macro-character #\# #\u #'sharp-url)