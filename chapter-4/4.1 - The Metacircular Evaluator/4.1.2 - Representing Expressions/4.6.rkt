#lang racket
(provide let->combination)
(provide let?)
(provide let-definitions-clause let-body let-parameters let-argument-expressions make-lambda)

; get the definitions part of the let construct
(define (let-definitions-clause exp)
  (cadr exp))

; get the lambda body part of the let constrcut
(define (let-body exp)
  (cddr exp))

; get the parameters part of the definitions clause from within the let construct
(define (let-parameters definitions-clause)
  (map car definitions-clause))

; get the actual argument expressions from the definitions clause from within the let construct
(define (let-argument-expressions definitions-clause)
  (map cadr definitions-clause))

; this procedure is used in `eval` when converting a
; (define (f x y) (+ x y)) statement
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; convert the let construct into a pair where the car is the lambda and the cons is a list of the
; arguments
(define (let->combination exp)
  (define definitions-clause (let-definitions-clause exp))
  (cons (make-lambda (let-parameters definitions-clause) (let-body exp))
        (let-argument-expressions definitions-clause)))

(define (tagged-list? exp val)
  (and (pair? exp) (eq? (car exp) val)))

(define (let? exp)
  (tagged-list? exp 'let))

; To add it into the eval, we would have to place the following line
; before the `application?` cond clause
; ((let? exp) (eval (let->combination exp) env))
