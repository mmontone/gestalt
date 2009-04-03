(in-package :gestalt)

(define-condition gestalt-condition ()
  ())

(define-condition url-dispatch-error (error gestalt-condition)
  ())

(define-condition session-not-found-error (error gestalt-condition)
  ())

(define-condition invalid-continuation-error (error gestalt-condition)
  ())