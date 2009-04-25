#|

In pwb we had differents politics for events handling. For example, deferred, only-once and just-in-time.

Maybe we should do the same here.

Stratified design for events handing:
1) Callbacks through method invocation
2) Callbacks through wlambda (to avoid memory-leaking)
3) Dataflow through dataflow syntax and MOP plugging.
4) Virtual trees + coherent reactions ???

|#
