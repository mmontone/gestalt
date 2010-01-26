(require 'asdf)
#+sbcl
(require :sb-posix)

(defpackage :gestalt.doc
  (:nicknames :gst.doc)
  (:use :cl))

(in-package :gestalt.doc)

(defparameter +references-dir-path+ 
  (merge-pathnames 
   #p"doc/references/"
   (asdf:component-pathname (asdf:find-system 'gestalt))))

(defparameter +docstrings-path+
  (merge-pathnames 
   #p"doc/docstrings.lisp"
   (asdf:component-pathname (asdf:find-system 'gestalt))))

(defparameter +docs-path+
  (merge-pathnames 
   #p"doc/"
   (asdf:component-pathname (asdf:find-system 'gestalt))))

(defparameter +texinfo-file+
  (merge-pathnames
   #p"gestalt.texinfo"
   +docs-path+))

(sb-posix:chdir +references-dir-path+)
(load +docstrings-path+)

(sb-texinfo:generate-includes +references-dir-path+
			      (find-package 'gst.view)
			      (find-package 'gst.view.xml)
			      (find-package 'gst.view.html)
			      (find-package 'gst.view.templates)
			      (find-package 'gst.view.dom))
(sb-posix:chdir +docs-path+)
(sb-ext:run-program "/usr/bin/texi2html" (list +texinfo-file+))
(sb-ext:run-program "/usr/bin/texi2pdf" (list +texinfo-file+))
(cl-user::quit)