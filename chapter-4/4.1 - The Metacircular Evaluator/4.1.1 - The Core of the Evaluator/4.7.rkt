#lang racket
(require "4.6.rkt")
(provide let*->nested-lets)

(define (is-last-definition? definitions-clause) (null? (cdr definitions-clause)))

(define (make-let definitions body)
    (cons 'let 
        (cons definitions body))

(define (let*->nested-lets exp)
    (define definitions-clause (let-definitions-clause exp))
    (if (is-last-definition? definitions-clause)
        exp
        (make-let (list (car definitions-clause))
            (let*->nested-lets (make-let (cdr definitions-clause) (let-body exp))))))