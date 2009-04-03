(require :hunchentoot)
(require :parenscript)

(defpackage :test
  (:use :common-lisp :parenscript))

(in-package :test)

(defvar *make-instance-defs* (make-hash-table :test 'equal))

(defclass cell ()
  ((value :accessor value))
   
   )

(defclass weakref ()
  ((object))
  )

(defmethod weakref-object (weakref)
  (

(defmacro weak-lambda (args &rest body)
  ())

   
(make-instance 'button :on-click (weak-lambda () (print "This is a great object: ~A" @object)))


  

(defpsmacro defclass (name superclass slots &rest methods)
  `(defun ,name ()
       ,(when superclass
	    `(setf this.inherit-from ,superclass))
       ,@(loop for slot in slots
	    collect
	      (progn
		(let* ((slot-attributes (cdr slot))
		       (slot-v (if (getf slot-attributes :value)
				   (getf slot-attributes :value)
				   nil)))
		  `(setf (slot-value this ',(car slot)) ,slot-v))))
       ,@(loop for method in methods
	      collect
	      (destructuring-bind
		    (defmethod-keyword
		     method-name
			method-args
		      &body method-body) method
		(declare (ignore defmethod-keyword))
		(if (string-equal method-name name)
		    ;; This is the constructor
		    
		    (progn
		      (setf (gethash name *make-instance-defs*)
			    method-args)
		      (let ((constructor-args (gensym "CONSTRARGS")))
			`(setf (slot-value this ',method-name)
			       (lambda (,constructor-args)
				 (progn
				   ,@(loop for arg in method-args
					  collect `(setf ,arg (aref ,constructor-args ,(string arg))))
			      ,@method-body)))))
		  ;; Else, this is a normal method
		  `(setf (slot-value this ',method-name)
			 (lambda ,method-args
			   ,@method-body)))))))
	    

(defun js-lib ()
  (ps
    (defclass my-class (super)
      ((x :value 2)
       (y :value 3)
       (z))
      (defmethod hello (who is) (print "hello"))
      (defmethod my-class (name test)
	(print "This is the constructor"))
      )
    
    ))

(defpage page
  (:scripts "dojo.js")
  (:embed js-lib dojo-bindings)
  )

(defun yui-bindings ()
  
  (setf YAHOO.example.container.simpledialog1 
	    (new YAHOO.widget.simple-dialog "simpledialog1"  
	             '(:width "300px" 
	               :fixedcenter t 
	               :visible f 
	               :draggable f 
	               :close t
	               :text "Do you want to continue?" 
	               :icon: YAHOO.widget.SimpleDialog.ICON_HELP
	               :constraintoviewport: t
	               :buttons: [ { text:"Yes", handler:handleYes, isDefault:true }, 
	                          { text:"No",  handler:handleNo } ] 
	             } )))

// Define various event handlers for Dialog 
var handleYes = function() { 
	    alert("You clicked yes!"); 
	    this.hide(); 
	}; 
	 
	var handleNo = function() { 
	    this.hide(); 
	};

(call (make-instance 'yui-dialog
		     :width 300
		     :fixedcenter t
		     :visible nil
		     :draggable nil
		     :close t
		     :text "Do you want to continue"
		     :icon :help
		     :contraintoviewport t
		     :buttons `(("yes" :on-click (lambda () (alert ("You clicked yes!!"))))
				("no" :on-click (lambda () (alert ("You clicked no!!")))))))

(defun zip (list)
  (when (equal (length list) 1)
    (error "the list has an odd number of elements"))
  (if (null list)
      nil
    (let ((x (car list))
	  (y (cadr list))
	  (rest (cddr list)))
      (cons (cons x y) (zip rest)))))

(defpsmacro make-instance (quoted-class-name &rest init-args)
  (let ((class-name (cadr quoted-class-name)))
    (when (not (gethash class-name *make-instance-defs*))
      (error "class ~A has not been declared yet" class-name))
    (let ((init-array (gensym "INITARRAY")))
      `(progn
	 ,@(loop for (key . value) in (zip init-args)
		 collect `(setf (aref ,init-array ,(string key)) ,value))
	 (new  (,class-name ,init-array))))))

      
(defun message-dialog-call ()
  (ps
    (setf msg-dialog (message-dialog))
    (setf (slot-value msg-dialog 'text) "This is a test man")
    (self.call msg-dialog aa)
    (setf other-msg-dialog (message-dialog))
    (setf (slot-value msg-dialog 'text) "Now we are done")
    (setf msg-dialog.text "Now we are done")
    (call other-msg-dialoga asdf)
    (setf my-object (make-instance 'my-class))))