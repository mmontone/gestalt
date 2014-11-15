(in-package :consistent-object.test)

(def-suite co.suite)

(defun run-tests ()
  (run 'co.suite))

(in-suite co.suite)

(defclass person ()
  ((name :initarg :name
	 :accessor name
	 :initform "")
   (lastname :initarg :lastname
	     :accessor lastname
	     :initform ""))
  (:metaclass consistent-object-class)
  (:documentation "A person"))

(defmethod validate ((person person))
  (ensure (and (stringp (name person))
	       (> (length (name person)) 0))
	  "Provide a name for ~A" person)
  (ensure (and (stringp (lastname person))
	       (> (length (lastname person)) 0))
	  "Provide a lastname for ~A" person))

(test consistent-object-test
      ;; The following fails:
      (signals consistency-error
	       (make-instance 'person))
      ;; The following fails:
      (signals consistency-error
	(let (person)
	  (suspending-consistency-for (person)
	    (setf person (make-instance 'person))
	    (setf (name person) "Mariano")
	    person)))

      ;; The following fails:
      (signals consistency-error
	       (let ((person (make-instance 'person)))
		 (setf (name person) "Mariano")
		 (setf (lastname person) "Montone")))

      ;; The following works:
      (finishes
	(let (person)
	  (suspending-consistency-for (person)
	    (setf person (make-instance 'person))
	    (setf (name person) "Mariano")
	    (setf (lastname person) "Montone")
	    person)))

      ;; Test the result is correct:
      (let (person)
	(let ((p (suspending-consistency-for (person)
		   (setf person (make-instance 'person))
		   (setf (name person) "Mariano")
		   (setf (lastname person) "Montone")
		   person)))
	  (is (name p) "Mariano")
	  (is (lastname p) "Montone"))))