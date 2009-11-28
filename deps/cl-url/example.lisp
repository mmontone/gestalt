;; example.lisp
(defparameter *url* (make-instance 'generic-url
				   :port 80
				   :parameters '(:a 44 :b 55)
				   :fragment "test"))
