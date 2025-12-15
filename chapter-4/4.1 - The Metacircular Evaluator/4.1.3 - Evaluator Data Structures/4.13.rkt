#lang sicp

#|
4.13

Scheme allows us to create new bindings for
variables by means of define, but provides no way to get
rid of bindings. Implement for the evaluator a special form
make-unbound! that removes the binding of a given symbol
from the environment in which the make-unbound! expres-
sion is evaluated. This problem is not completely specified.
For example, should we remove only the binding in the first
frame of the environment? Complete the specification and
justify any choices you make.

---

For this exercise, I will specify that the make-unbound! procedure
should only unbind a variable in the current frame. The reason is
that otherwise, a procedure can cause variables from outer frames
to become unbound, causing very difficult to track bugs. The "owner"
of the variable is the one who created it, only they should be able to
delete it.

This is different than the specification for set-variable-value! which
traverses the entire environment, true. However, it could be argued that 
unbinding a variable is more bug-prone than changing the value of a variable.
Furthermore, it might even be better to change the specification of set-variable-value!.

If the variable does not exist, an unbound identifier error will be thrown
|#

(define (set-frame-variables! frame vars) (set-car! frame vars))
(define (set-frame-values! frame vals) (set-cdr! frame vals))

(define (unbind-variable-from-frame! var val frame)
    (let ((index (index-of (frame-variables frame) var)))
        (set-frame-variables! frame (remove var (frame-variables frame))
        (set-frame-values! frame (remove val (frame-values frame) index)))))

(define (make-unbound! var env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (if (null? vars)
                (error "MAKE-UNBOUND! Unbound variable" var)
                    (if (eq? var (first-variable vars)) (unbind-variable-from-frame! var (first-value val) frame)
                        (scan (rest-of-variables vars) (rest-of-values vals)))
            ))
        (scan (frame-variables frame)
            (frame-values frame))))            