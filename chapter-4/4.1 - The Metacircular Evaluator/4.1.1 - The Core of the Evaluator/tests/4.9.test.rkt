#lang racket
(require rackunit)
(require "../4.9.rkt")

(define do-expression
 '(do (lambda ()
        (display "LOOP")
         (newline))
    (lambda ()
       #f)))
(define do-derived
  '(let loop ((proc (lambda ()
                           (display "LOOP")
                           (newline)))
              (pred (lambda ()
                      #f)))
     (proc)
     (if (pred) (loop) 'done)))
(check-equal? (do->combination do-expression) do-derived)