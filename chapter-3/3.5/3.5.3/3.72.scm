#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "exploiting-the-stream-paradigm.scm")
(#%require "3.70.scm") ; merge-weighted

(define (square x) (* x x))
(define (weighing-function pair) (+ (square (car pair)) (square (cadr pair))))

(define (square-sum-three-ways s)
  (if (stream-null? s)
      the-empty-stream
      (let ((w1 (weighing-function (stream-car s)))
            (w2 (weighing-function (stream-car (stream-cdr s))))
            (w3 (weighing-function (stream-car (stream-cdr (stream-cdr s))))))
        (if (= w1 w2 w3)
            (cons-stream (list (stream-car s) (stream-car (stream-cdr s)) (stream-car (stream-cdr (stream-cdr s))) w1)
                         (square-sum-three-ways (stream-cdr (stream-cdr (stream-cdr s)))))
            (square-sum-three-ways
             (stream-cdr s))))))

(define s (square-sum-three-ways (pairs-weighted integers integers weighing-function)))

(ss s 10)