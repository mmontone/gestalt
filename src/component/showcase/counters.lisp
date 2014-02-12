(in-package :gestalt)

(defcomponent counters-showcase ()
  ())

(defmethod initialize-instance :after ((counters counters-showcase) &rest initargs)
  (add-component counters (gensym) (make-instance 'counter :value 1))
  (add-component counters (gensym) (make-instance 'counter :value 2))
  (add-component counters (gensym) (make-instance 'counter :value 3)))
