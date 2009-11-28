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
	   #:*default-url-defaults*))

(defpackage cl-url.test
  (:use :cl :cl-url :fiveam)
  (:export #:run-tests))