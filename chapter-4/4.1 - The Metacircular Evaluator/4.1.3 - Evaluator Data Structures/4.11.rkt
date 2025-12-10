#lang sicp
#|
4.11

Instead of representing a frame as a pair of
lists, we can represent a frame as a list of bindings, where
each binding is a name-value pair. Rewrite the environment
operations to use this alternative representation.
|#

; An environment is a list of frames
(define (enclosing-environement env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each Frame is a list of pairs
(define (make-binding var val) (cons var val))
(define (binding-var binding) (car binding))
(define (binding-val binding) (cdr binding))
(define (make-frame bindings) bindings)

(define (first-binding frame) (car frame))
(define (rest-of-bindings frame) (cdr frame))
(define (first-var frame) (car (first-binding frame)))
(define (first-val frame) (cdr (first-binding frame)))
(define (set-binding-val! binding val) (set-cdr! binding val))
(define (add-binding-to-frame! binding frame)
  (set-car! frame (cons binding frame)))


; Extending environments with a new frame
(define (extend-environment bindings base-env)
  (cons (make-frame bindings) base-env))

; Looking up a variable
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
              (env-loop (enclosing-environement env)))
            ((eq? var (first-var frame)) (first-val frame))
            (else (scan (rest-of-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
    (env-loop env))

; Setting a variable (aka adjusting an existing variable's value)
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
              (env-loop (enclosing-environement env)))
            ((eq? var (first-var frame)) (set-binding-val! (first-binding frame) val))
            (else (scan (rest-of-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
    (env-loop env))

; Defining a new variable
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings) (add-binding-to-frame! (make-binding var val) frame))
            ((eq? var (binding-var (first-binding bindings)))
              (set-binding-val! (first-binding bindings) val))
            (else (scan (rest-of-bindings frame)))))
    (scan frame)))

; tests
(define base-env the-empty-environment)
(define bindings (list (make-binding 'x 1) (make-binding 'y 2)))
(define env1 (extend-environment bindings base-env))
; (eq? (lookup-variable-value 'x env1) 1)
; (eq? (lookup-variable-value 'y env1) 2)

(define new-bindings (list (make-binding 'x 3) (make-binding 'y 4)))
(define env2 (extend-environment new-bindings env1))
; (eq? (lookup-variable-value 'x env2) 3)
; (eq? (lookup-variable-value 'y env2) 4)
(set-variable-value! 'x 5 env2)
; (eq? (lookup-variable-value 'x env2) 5)
; TODO: define-variable runs indefinitely
(define-variable! 'z 6 env2)
(eq? (lookup-variable-value 'z env2) 6)
(define-variable! 'y 7 env2)
(eq? (lookup-variable-value 'y env2) 7)