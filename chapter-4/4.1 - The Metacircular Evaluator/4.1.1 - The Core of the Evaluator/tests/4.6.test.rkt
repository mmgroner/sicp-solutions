#lang racket
(require rackunit)
(require "../4.6.rkt")

(define simple-let-expression '(let ((x 1) (y 2)) (+ x y)))
(check-equal? (let->combination simple-let-expression) 
    (cons
        (list 'lambda '(x y) '(+ x y))
        (list 1 2)
    )
)
(check-equal? (let? simple-let-expression) #t)