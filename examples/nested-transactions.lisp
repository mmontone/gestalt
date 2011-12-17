(require :ele-bdb)
(require :hunchentoot)
(require :cl-who)

(defpackage tapp
  (:use :cl :elephant :hunchentoot :cl-who))

(in-package :tapp)

(setf hunchentoot:*catch-errors-p* nil)

(setf hunchentoot:*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT*
      (flexi-streams:make-external-format :utf8 :eol-style :lf))

(start (make-instance 'acceptor :port 9091))

(setf elephant::*default-mvcc* t)

(defparameter *test-db-spec*
  '(:BDB "/home/marian/tmp/tapp2/"))
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
			       :value (lastname person))))
		     ))
		   (:input :type "submit"
			   :value "submit")))))))

(define-easy-handler (save-person :uri "/save-person")
    (id name lastname)
  (start-session)
  (let ((success nil))
    (catch 'elephant::transaction
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
	(setf success
	      (let ((address (address person)))
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
		      (:td
		       (:label (str "Street:")))
		      (:td
		       (:label (str (street address)))))
		     (:tr
		      (:td
		       (:label (str "Number:")))
		      (:td
		       (:label (str (street-number address)))))
		     (:tr
		      (:td
		       (:label (str "City:")))
		      (:td
		       (:label (str (city address)))))
		     (:tr
		      (:td
		       (:label (str "Country:")))
		      (:td
		       (:label (str (country address)))))
		     (:tr
		      (:td
		       (:label (str "Address:")))
		      (:td
		       (:a :href (format nil "/edit-address?id=~A&tid=~A"
					 (id (address person))
					 tx-id)
			   (str "Edit"))))
		     )
		    (:tr
		     (:td (:a :href (format nil "/commit?tid=~A" tx-id)
			      (str "Save")))
		     (:td (:a :href (format nil "/discard?tid=~A" tx-id)
			      (str "Cancel")))))))))))
    (if (not success)
	(with-html-output-to-string (s)
	  (htm
	   (:b (str "Error"))))
	success)))

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
	  :accessor email)
   (address :initarg :address
	    :initform (make-instance 'address)
	    :accessor address))
  (:metaclass persistent-metaclass))

(defclass address ()
  ((id :initarg :id :initform (next-address-id)
       :accessor id :index t)
   (street :initarg :street :initform nil
	   :accessor street)
   (number :initarg :number :initform nil
	   :accessor street-number)
   (city :initarg :city :initform nil
	 :accessor city)
   (country :initarg :country :initform nil
	    :accessor country))
  (:metaclass persistent-metaclass))

(define-easy-handler (edit-address :uri "/edit-address") (id tid)
  (start-session)
  (let ((address (first (get-instances-by-value 'address 'id (parse-integer id)))))
    (with-html-output-to-string (s)
      (htm (:form :action "/save-address"
		  :method "post"
		  (:fieldset
		   (:input :type "hidden"
			   :name "id"
			   :value id)
		   (:input :type "hidden"
			   :name "tid"
			   :value tid)
		   (:table
		    (:tbody
		     (:tr
		      (:td
		       (:label (str "Street:")))
		      (:td
		       (:input :type "text"
			       :name "street"
			       :value (street address))))
		     (:tr
		      (:td
		       (:label (str "Number:")))
		      (:td
		       (:input :type "text"
			       :name "number"
			       :value (street-number address))))
		     (:tr
		      (:td
		       (:label (str "City:")))
		      (:td
		       (:input :type "text"
			       :name "city"
			       :value (city address))))
		     (:tr
		      (:td
		       (:label (str "Country:")))
		      (:td
		       (:input :type "text"
			       :name "country"
			       :value (country address))))
		     ))
		   (:input :type "submit"
			   :value "submit")))))))

(define-easy-handler (save-address :uri "/save-address")
    (id tid street number city country)
  (start-session)
  (let ((success nil))
    (catch 'elephant::transaction
      (let* ((address (first (get-instances-by-value 'address 'id
						     (parse-integer id))))
	     (parent-tx (gethash (parse-integer tid) *transactions*))
	     (tx-id (incf *transaction-id-seq*))
	     (tx (controller-start-transaction *store-controller*))
	     (elephant::*current-transaction* (list *store-controller*
						    tx (second parent-tx))))
	(setf (gethash tx-id *transactions*)
	      elephant::*current-transaction*)
	(setf (street address) street)
	(setf (street-number address) number)
	(setf (city address) city)
	(setf (country address) country)
	(setf success
	      (with-html-output-to-string (s)
		(htm
		 (:table
		  (:tbody
		   (:tr
		    (:td
		     (:label (str "Street:")))
		    (:td
		     (:label (str (street address)))))
		   (:tr
		    (:td
		     (:label (str "Number:")))
		    (:td
		     (:label (str (street-number address)))))
		   (:tr
		    (:td
		     (:label (str "City:")))
		    (:td
		     (:label (str (city address)))))
		   (:tr
		    (:td
		     (:label (str "Country:")))
		    (:td
		     (:label (str (country address)))))
		   (:tr
		    (:td (:a :href (format nil "/commit?tid=~A" tx-id)
			     (str "Save")))
		    (:td (:a :href (format nil "/discard?tid=~A" tx-id)
			     (str "Cancel")))))))))))
    (if (not success)
	(with-html-output-to-string (s)
	  (htm
	   (:b (str "Error"))))
	success)))

(defmethod title ((person person))
  (format nil "~A ~A" (name person) (lastname person)))

(defun next-person-id ()
  (prog1
      (get-from-root :persons-id-seq)
    (add-to-root :persons-id-seq (1+ (get-from-root :persons-id-seq)))))

(defun next-address-id ()
  (prog1
      (get-from-root :address-id-seq)
    (add-to-root :address-id-seq (1+ (get-from-root :address-id-seq)))))

(defun init-db ()
  (add-to-root :persons-id-seq 1)
  (add-to-root :address-id-seq 1)
  (make-instance 'person :name "Mariano" :lastname "Montone"))