(require :ele-bdb)
(require :hunchentoot)
(require :cl-who)

(defpackage tapp
  (:use :cl :elephant :hunchentoot :cl-who))

(in-package :tapp)

(setf hunchentoot:*catch-errors-p* nil)

(setf hunchentoot:*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT*
      (flexi-streams:make-external-format :utf8 :eol-style :lf))

(start (make-instance 'acceptor :port 9090))

(setf elephant::*default-mvcc* t)

(defparameter *test-db-spec*
  '(:BDB "/home/marian/tmp/tapp/"))
(defparameter *transactions* (make-hash-table))
(defparameter *transaction-id-seq* 1)

(open-store *test-db-spec*)

(define-easy-handler (root :uri "/") ()
  (start-session)
  (with-html-output-to-string (s)
    (loop for person in (get-instances-by-class 'person)
       do (htm (:a :href (format nil "/edit-person?id=~A" (id person))
		   (:b (str (title person))))
	       :br))))

(define-easy-handler (edit-person :uri "/edit-person") (id)
  (start-session)
  (let ((person (first (get-instances-by-value 'person 'id (parse-integer id)))))
    (with-html-output-to-string (s)
      (htm (:form :action "/save-person"
		  :method "post"
		  (:fieldset
		   (:input :type "hidden"
			   :name "id"
			   :value id)
		   (:table
		    (:tbody
		     (:tr
		      (:td
		       (:label (str "Name:")))
		      (:td
		       (:input :type "text"
			       :name "name"
			       :value (name person))))
		     (:tr
		      (:td
		       (:label (str "Lastname:")))
		      (:td
		       (:input :type "text"
			       :name "lastname"
			       :value (lastname person))))))
		   (:input :type "submit"
			   :value "submit")))))))

(define-easy-handler (save-person :uri "/save-person") (id name lastname)
  (start-session)
  (let* ((person (first (get-instances-by-value 'person 'id
					       (parse-integer id))))
	 (tx-id (incf *transaction-id-seq*))
	 (tx (controller-start-transaction *store-controller*))
	 (elephant::*current-transaction* (list *store-controller*
						tx nil)))
    (setf (gethash tx-id *transactions*)
	  elephant::*current-transaction*)
    (setf (name person) name)
    (setf (lastname person) lastname)
    (with-html-output-to-string (s)
      (htm
       (:table
	(:tbody
	 (:tr
	  (:td
	   (:label (str "Name:")))
	  (:td
	   (:label (str (name person)))))
	 (:tr
	  (:td
	   (:label (str "Lastname:")))
	  (:td
	   (:label (str (lastname person)))))
	 (:tr
	  (:td (:a :href (format nil "/commit?tid=~A" tx-id)
		   (str "Save")))
	  (:td (:a :href (format nil "/discard?tid=~A" tx-id)
		   (str "Cancel"))))))))))

(define-easy-handler (commit :uri "/commit") (tid)
  (let ((tid (parse-integer tid)))
    (with-html-output-to-string (s)
      (let ((tx (gethash tid *transactions*)))
	(controller-commit-transaction *store-controller* (second tx))
	(htm (:b (str (format nil "Committing transaction ~A" tx)))
	     (:a :href "/" (:b (str "Go home"))))))))

(define-easy-handler (discard :uri "/discard") (tid)
  (let ((tid (parse-integer tid)))
    (with-html-output-to-string (s)
      (let ((tx (gethash tid *transactions*)))
	(controller-abort-transaction *store-controller* (second tx))
	(htm (:b (str (format nil "Discarding transaction ~A" tx)))
	     (:a :href "/" (:b (str "Go home"))))))))

(defclass person ()
  ((id :initarg :id :initform (next-person-id)
       :accessor id :index t)
   (name :initarg :name :initform nil
	 :accessor name)
   (lastname :initarg :lastname :initform nil
	     :accessor lastname)
   (email :initarg :email :initform nil
	  :accessor email))
  (:metaclass persistent-metaclass))

(defmethod title ((person person))
  (format nil "~A ~A" (name person) (lastname person)))

(defun next-person-id ()
  (prog1
      (get-from-root :persons-id-seq)
    (add-to-root :persons-id-seq (1+ (get-from-root :persons-id-seq)))))

(get-from-root :persons)
(add-to-root :persons (make-hash-table))
(add-to-root :persons-id-seq 1)