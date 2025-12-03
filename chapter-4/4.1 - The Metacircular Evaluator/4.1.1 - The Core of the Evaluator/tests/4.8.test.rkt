#lang racket
(require rackunit)
(require "../4.8.rkt")

(define simple-let-expression
  '(let ((x 1)
         (y 2))
     (+ x y)))
(check-equal? (let->combination simple-let-expression)
              (cons (list 'lambda '(x y) '(+ x y)) (list 1 2)))
(define multi-line-body-let-expression
  '(let ((x 1)
         (y 2))
     (display x)
     (+ x y)))
(check-equal? (let->combination multi-line-body-let-expression)
              (cons (list 'lambda '(x y) '(display x) '(+ x y)) (list 1 2)))

(define named-let-expression
    '(let fib-iter ((a 1)
                    (b 0)
                    (count n))
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1)))))
; TODO: Read https://docs.scheme.org/schintro/schintro_66.html#SEC73
; Then update test before actually implementing the chance
(check-equal? (let->combination named-let-expression)
    (cons
        (list 'lambda '(a b count) 
                        '(define (fib-iter a b count)
                            (if (= count 0)
                                b
                                (fib-iter (+ a b) a (- count 1))))
                        '(fib-iter a b count))
        (list 1 0 'n)))
        