(defpackage dlist
  (:use :cl)
  (:export #:dlist
	   #:make-dlist
	   #:head
	   #:tail
	   #:insert-between
	   #:insert-before
	   #:insert-after
	   #:insert-head
	   #:insert-tail
	   #:remove-link
	   #:dlist-elements
	   #:do-dlist
	   #:map-dlist))

(in-package :dlist)

(defstruct (dlist
	     (:print-function print-dlist))
  head
  tail)

(defun print-dlist (dlist stream &rest args)
  (declare (ignore args))
  (print-unreadable-object (dlist stream :identity t :type t)
    (format stream "~A" (dlist-elements dlist))))

(defstruct (dlink
	     (:print-function print-dlink))
  content
  prev
  next)

(defun print-dlink (dlink stream &rest args)
  (declare (ignore args))
  (print-unreadable-object (dlink stream :type t :identity t)
    (format stream "~A" (dlink-content dlink))))
 
(defun insert-between (dlist before after data)
  "Insert a fresh link containing DATA after existing link BEFORE if not nil and before existing link AFTER if not nil"
  (let ((new-link (make-dlink :content data :prev before :next after)))
    (if (null before)
        (setf (dlist-head dlist) new-link)
        (setf (dlink-next before) new-link))
    (if (null after)
        (setf (dlist-tail dlist) new-link)
        (setf (dlink-prev after) new-link))
    new-link))
 
(defun insert-before (dlist dlink data)
  "Insert a fresh link containing DATA before existing link DLINK"
  (insert-between dlist (dlink-prev dlink) dlink data))
 
(defun insert-after (dlist dlink data)
  "Insert a fresh link containing DATA after existing link DLINK"
  (insert-between dlist dlink (dlink-next dlink) data))
 
(defun insert-head (dlist data)
  "Insert a fresh link containing DATA at the head of DLIST"
  (insert-between dlist nil (dlist-head dlist) data))
 
(defun insert-tail (dlist data)
  "Insert a fresh link containing DATA at the tail of DLIST"
  (insert-between dlist (dlist-tail dlist) nil data))
 
(defun remove-link (dlist dlink)
  "Remove link DLINK from DLIST and return its content"
  (let ((before (dlink-prev dlink))
        (after (dlink-next dlink)))
    (if (null before)
        (setf (dlist-head dlist) after)
        (setf (dlink-next before) after))
    (if (null after)
        (setf (dlist-tail dlist) before)
        (setf (dlink-prev after) before))))
 
(defun dlist-elements (dlist)
  "Returns the elements of DLIST as a list"
  (labels ((extract-values (dlink acc)
             (if (null dlink)
                 acc
                 (extract-values (dlink-next dlink) (cons (dlink-content dlink) acc)))))
    (reverse (extract-values (dlist-head dlist) nil))))

(defmacro do-dlist ((var dlist &optional result) &rest body)
  `(progn
     (map-dlist (lambda (,var)
		  ,@body)
		,dlist)
     ,result))

(defun map-dlist (function dlist)
  (mapcar function (dlist-elements dlist)))

;; Example:

(let ((dlist (make-dlist)))
  (insert-head dlist 1)
  (insert-tail dlist 4)
  (insert-after dlist (dlist-head dlist) 2)
  (let* ((next-to-last (insert-before dlist (dlist-tail dlist) 3))
         (bad-link (insert-before dlist next-to-last 42)))
    (remove-link dlist bad-link))
  (print (dlist-elements dlist)))

(let ((dlist (make-dlist)))
  (insert-head dlist 1)
  (insert-tail dlist 4)
  (insert-after dlist (dlist-head dlist) 2)
  (map-dlist #'1+ dlist))
