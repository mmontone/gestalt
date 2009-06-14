(defpackage :code-walker
  (:nicknames :cw)
  (:use :cl :gstutils :contextl :sb-mop)
  (:import-from :gstutils :dynamic-variable-symbol-p)
  (:import-from :arnesi :with-unique-names)
  (:import-from :anaphora :aprog1 :it)
  (:import-from :log5 :defcategory :start-sender :stream-sender :log-for :message)
  (:export
   #:code-walker
   #:*standard-code-walker*
   #:apply-code-walker
   #:define-code-walker-pattern
   #:define-code-walker-case))