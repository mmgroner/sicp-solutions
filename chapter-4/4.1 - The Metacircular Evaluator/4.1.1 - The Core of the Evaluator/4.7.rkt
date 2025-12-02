#lang racket
(require "4.6.rkt")
(provide let*->nested-lets)

(define (is-last-definition? definitions-clause) (null? (cdr definitions-clause)))

(define (make-let definitions body)
    (cons 'let 
        (cons definitions body)))

(define (let*->nested-lets exp)
    (define definitions-clause (let-definitions-clause exp))
    (define body (let-body exp))
    (cond ((null? definitions-clause) (make-let definitions-clause body))
          ((is-last-definition? definitions-clause) exp)
          (else 
            (make-let (list (car definitions-clause))
                (list (let*->nested-lets (make-let (cdr definitions-clause) (let-body exp))))))))
    