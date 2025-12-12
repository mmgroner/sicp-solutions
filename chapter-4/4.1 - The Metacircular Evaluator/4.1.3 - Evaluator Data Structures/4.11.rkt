#lang sicp
#|
4.11

Exercise:

Instead of representing a frame as a pair of
lists, we can represent a frame as a list of bindings, where
each binding is a name-value pair. Rewrite the environment
operations to use this alternative representation.

Solution Notes:

The key to solving this was to make frame a data structure,
not simply a list.

The reason is because if it's a list, we can't add a binding
properly. You can't set! frame (cons new-binding frame),
because that will only bind the local variable.

By turning frame into a data structure, it's possible to do so easily.

A key understanding that is reached here is the difference between
set! and set-car!/set-cdr!. The former (set!) rebinds the variable in
the current frame. The latter mutates the data structure that is being pointed
to (referenced) in memory.
|#

; An environment is a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; A frame is a tagged pair
; This allows us to modify the frame to extend it
; The cdr of a frame is a list of bindings
; Bindings are a pair of var and val
(define (make-binding var val) (cons var val))
(define (binding-var binding) (car binding))
(define (binding-val binding) (cdr binding))
(define (set-binding-val! binding val) (set-cdr! binding val))

(define (make-frame bindings) (cons 'frame bindings))
; (define (is-frame? frame) (tagged-list? frame 'frame))
;; tagged-list? is not defined here, so commenting it out
(define (frame-contents frame) (cdr frame))
(define (is-frame-empty? frame) (null? (frame-contents frame)))

(define (frame-contents-first-binding frame-contents) (car frame-contents))
(define (frame-contents-rest-of-bindings frame-contents) (cdr frame-contents))
(define (frame-contents-first-var frame-contents) (binding-var (frame-contents-first-binding frame-contents)))
(define (frame-contents-first-val frame-contents) (binding-val (frame-contents-first-binding frame-contents)))
(define (add-binding-to-frame! binding frame)
  (set-cdr! frame (cons binding (frame-contents frame))))


; Extending environments with a new frame
(define (extend-environment bindings base-env)
  (cons (make-frame bindings) base-env))

; Looking up a variable
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame-contents)
      (cond ((null? frame-contents)
              (env-loop (enclosing-environment env)))
            ((eq? var (frame-contents-first-var frame-contents)) (frame-contents-first-val frame-contents))
            (else (scan (frame-contents-rest-of-bindings frame-contents)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (frame-contents (first-frame env)))))
    (env-loop env))

; Setting a variable (aka adjusting an existing variable's value)
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame-contents)
      (cond ((null? frame-contents)
              (env-loop (enclosing-environment env)))
            ((eq? var (frame-contents-first-var frame-contents)) (set-binding-val! (frame-contents-first-binding frame-contents) val))
            (else (scan (frame-contents-rest-of-bindings frame-contents)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (frame-contents (first-frame env)))))
    (env-loop env))

; Defining a new variable
; TODO: this procedure is not adding the variable as expected..
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame-contents)
      (cond ((null? frame-contents) (add-binding-to-frame! (make-binding var val) frame))
            ((eq? var (frame-contents-first-var frame-contents))
              (set-binding-val! (frame-contents-first-binding frame-contents) val))
            (else (scan (frame-contents-rest-of-bindings frame-contents)))))
    (scan (frame-contents frame))))

; tests
(define base-env the-empty-environment)
(define bindings (list (make-binding 'x 1) (make-binding 'y 2)))
(define env1 (extend-environment bindings base-env))
(eq? (lookup-variable-value 'x env1) 1)
(eq? (lookup-variable-value 'y env1) 2)

(define new-bindings (list (make-binding 'x 3) (make-binding 'y 4)))
(define env2 (extend-environment new-bindings env1))
(eq? (lookup-variable-value 'x env2) 3)
(eq? (lookup-variable-value 'y env2) 4)
(set-variable-value! 'x 5 env2)
(eq? (lookup-variable-value 'x env2) 5)
(define-variable! 'z 6 env2)
(eq? (lookup-variable-value 'z env2) 6)
(define-variable! 'y 7 env2)
(eq? (lookup-variable-value 'y env2) 7)