(progn
(progn
  (require :xmlisp)
  (require :fiveam)
  (require :gst.util)
  (require :cl-json)
  (require :parenscript)
  (require :alexandria)
  (require :closer-mop)
  (require :cl-who)
  (require :arnesi))

(progn
  (load #p"/home/marian/src/lisp/gestalt/src/view/package.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/xml.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/tracked-xml-node.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/dom.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/html.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/view.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/templates.lisp")
  (load #p"/home/marian/src/lisp/gestalt/src/view/test.lisp")))

(gst.view.test::RUN! 'gst.view.test::view-test-suite)

