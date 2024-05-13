#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "exploiting-the-stream-paradigm.scm")
(#%require "3.70.scm") ; merge-weighted

(define (cube x) (* x x x))
(define (weighing-function pair) (+ (cube (car pair)) (cube (cadr pair))))

(define (ramanujan s)
  (if (stream-null? s)
      the-empty-stream
      (let ((w1 (weighing-function (stream-car s)))
            (w2 (weighing-function (stream-car (stream-cdr s)))))
        (if (= w1 w2)
            (cons-stream (list (stream-car s) (stream-car (stream-cdr s)) w1)
                         (ramanujan (stream-cdr (stream-cdr s))))
            (ramanujan
             (stream-cdr s))))))

(define r (ramanujan (pairs-weighted integers integers weighing-function)))
(ss r 10)