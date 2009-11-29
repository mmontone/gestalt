(defpackage cl-url
  (:use :cl)
  (:export #:url
	   #:generic-url
	   #:make-url
	   #:protocol
	   #:hostname
	   #:port
	   #:path
	   #:program
	   #:parameters
	   #:fragment
	   #:protocol-name
	   #:define-url-protocol
	   #:url-string
	   #:merge-urls
	   #:copy-url
	   #:url-equal
	   #:extract-url
	   #:read-url
	   #:*default-url-defaults*
	   #:merge-conflict))