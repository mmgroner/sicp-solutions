#lang racket
(require rackunit)
(require "../4.7.rkt")

(define let*-expression '(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
                        (* x z)))

(define expected-outcome
    '(let ((x 3))
        (let ((y (+ x 2)))
            (let ((z (+ x y 5)))
                (* x z)))))

(check-equal? (let*->nested-lets let*-expression) expected-outcome)

(define let*-with-no-definitions '(let* () (+ 1 2)))
(check-equal? (let*->nested-lets let*-with-no-definitions)
    '(let () (+ 1 2)))