#lang sicp

#|
4.12

The procedures set-variable-value!, define-
variable! and lookup-variable-value can be expressed
in terms of more abstract procedures for traversing the en-
vironment structure. Define abstractions that capture the
common patterns and redefine the three procedures in terms
of these abstractions.
|#

; Environments

; An environment is a list of frames
(define (enclosing-environement env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each Frame is a pair os lists
; A list of variables, and a list of values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
; Extra getters here
(define (first-variable variables) (car variables))
(define (first-value values) (car values))
(define (rest-of-variables variables) (cdr variables))
(define (rest-of-values values) (cdr values))
(define (set-first-val! values new-val) (set-car! values new-val))

; Extending environments with a new frame
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Looking up a variable
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
              (env-loop (enclosing-environement env)))
            ((eq? var (first-variable vars)) (first-value vals))
            (else (scan (rest-of-variables vars) (rest-of-values vals))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))))))
    (env-loop env))

; Setting a variable (aka adjusting an existing variable's value)
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
              (env-loop (enclosing-environement env)))
            ((eq? var (first-variable vars)) (set-first-val! vals val))
            (else (scan (rest-of-variables vars) (rest-of-values vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env))
            (scan (frame-variables frame)
                (frame-values frame))))))
    (env-loop env))

; Defining a new variable
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (first-variable vars)) (set-first-val! vals val))
            (else (scan (rest-of-variables vars) (rest-of-values vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))