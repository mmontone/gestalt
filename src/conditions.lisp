(in-package :gestalt)

(define-condition gestalt-condition ()
  ())

(define-condition url-dispatch-error (serious-condition gestalt-condition)
  ())

(define-condition session-not-found-error (serious-condition gestalt-condition)
  ())

(define-condition invalid-continuation-error (serious-condition gestalt-condition)
  ())