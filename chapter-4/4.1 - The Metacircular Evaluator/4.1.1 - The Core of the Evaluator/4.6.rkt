#lang racket
(provide let->combination)
(provide let?)

(define (let-definitions-clause exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-parameters definitions-clause) (map car definitions-clause))    
(define (let-argument-expressions definitions-clause) (map cadr definitions-clause))

(define (let->combination exp)
    (define definitions-clause (let-definitions-clause exp))
    (cons
        (list 'lambda (let-parameters definitions-clause) (let-body exp)) 
        (let-argument-expressions definitions-clause))
)

(define (tagged-list? exp val)
    (and (pair? exp) (eq? (car exp) val)))

(define (let? exp) (tagged-list? exp 'let))

; To add it into the eval, we would have to place the following line
; before the `application?` cond clause
; ((let? exp) (eval (let->combination exp) env))

