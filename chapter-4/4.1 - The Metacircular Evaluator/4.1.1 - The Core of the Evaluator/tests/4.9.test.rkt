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

(define for-expression
  '(for 
       (1 2 3 4 5)
     (lambda (elm)
       (display "Element is: ")
       (display elm)
       (newline))
     ))
(define for-derived
  '(let loop (
              (seq (1 2 3 4 5))
              (proc (lambda (elm)
                      (display "Element is: ")
                      (display elm)
                      (newline))))
     (if (null? seq)
         'done
         (begin
           (proc (car seq))
           (loop (cdr seq))))))
(check-equal? (for->combination for-expression) for-derived)

(define while-expression
  '(while (lambda () #f) (lambda () (display "LOOP") (newline))))
(define while-derived
  '(let loop
     ((pred (lambda () #f))
      (proc (lambda () (display "LOOP") (newline))))
     (if (pred)
         (begin
           (proc)
           (loop))
         'done)))
(check-equal? (while->combination while-expression) while-derived)

(define until-expression
  '(until (lambda () #t) (lambda () (display "LOOP") (newline))))
(define until-derived
  '(let loop ((pred (lambda () #t))
              (proc (lambda () (display "LOOP") (newline))))
     (if (not pred)
         (begin
           (proc)
           (loop))
         'done)))