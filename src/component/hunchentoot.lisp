(in-package :gestalt)

(defclass gestalt-request (hunchentoot:request)
  ())

#+nil(defmethod hunchentoot:process-request :around ((request gestalt-request))
  (let ((*standard-special-page-p* t))
    (call-next-method)))

#+nil(defmethod hunchentoot:header-in ((name (eql :host)) (request gestalt-request))
  (or (hunchentoot:header-in :x-forwarded-host request)
      (call-next-method)))


(defclass gestalt-acceptor (hunchentoot:acceptor) 
  ((application :initarg :application
		:accessor application))
  (:default-initargs
   :request-class 'gestalt-request
   :error-template-directory nil
   :access-log-destination nil
   :message-log-destination nil))

(defun request-hostname-port (acceptor request)
  (let ((host (cdr (assoc :host (hunchentoot:headers-in request)))))
    (if (find #\: host)
        (destructuring-bind (hostname port) (ppcre:split ":" host)
          (cons hostname
                (parse-integer port)))
        (cons host (hunchentoot:acceptor-port acceptor)))))

(defun gestalt-dispatch-request (acceptor request)
  (if (equalp (hunchentoot:script-name request) "/")
      ;; Render the application root component
      (aif (hunchentoot:get-parameter "_z")
	   (let ((state (read-from-string (decode-string it))))
	     (let ((application
		    (unserialize-application-from-uri state)))
		 (render application)))
	     ;; else, there's no state to unserialize,
	     ;; render the root component
	     (render (application acceptor)))
	;; else, try to match an action
	(let ((action-name (intern (subseq (hunchentoot:script-name request) 1) :gestalt)))
	  (if (and (ignore-errors (symbol-function action-name))
		   (get action-name :action-p))
	      (let ((state (decode-string (hunchentoot:get-parameter "_z"))))
		(let ((application
		       (unserialize-application-from-uri state)))
		  (let ((*application* application))
		    ;; We found the action, execute it
		    (funcall (unserialize-action action-name
						 (decode-string (hunchentoot:get-parameter "_a"))))
		    ;; Render the resulting application
		    (render application))))
	      ;; else, error, no matching action
	      (error "No matching action")))))
  
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor gestalt-acceptor) request)
  (gestalt-dispatch-request acceptor request))

(defun start-application (application &key (host "localhost") (port 80))
  (hunchentoot:start (make-instance 'gestalt-acceptor
				    :application application
				    :port port
				    :persistent-connections-p nil)))
