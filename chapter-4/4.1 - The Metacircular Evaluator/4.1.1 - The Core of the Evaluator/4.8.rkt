#lang racket
(provide let->combination)
(provide let-definitions-clause let-body let-parameters let-argument-expressions make-lambda)

(define (is-named-let? exp) (symbol? (cadr exp)))

(define (let-definitions-clause exp)
  (if (is-named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (is-named-let? exp)
    (cdddr exp)
    (cddr exp)))

(define (let-parameters definitions-clause)
  (map car definitions-clause))

(define (let-argument-expressions definitions-clause)
  (map cadr definitions-clause))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let->combination exp)
  (define definitions-clause (let-definitions-clause exp))
  (cons (make-lambda (let-parameters definitions-clause) (let-body exp))
        (let-argument-expressions definitions-clause)))
