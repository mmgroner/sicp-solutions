#lang sicp
(#%require "streams.scm")
(#%provide (all-defined))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
; (ss fibs)

(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes-sieve (sieve (integers-starting-from 2)))
; (ss primes-sieve)
