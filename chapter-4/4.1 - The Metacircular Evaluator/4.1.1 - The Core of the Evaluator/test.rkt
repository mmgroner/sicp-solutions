#lang racket
(define (make-let definitions body)
    (list 'let definitions body))

(define let-exp (make-let '((x 2) (y 3)) (display x) (+ x y))))

(display let-exp)

; (eval let-exp)
