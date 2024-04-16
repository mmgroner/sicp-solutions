#lang sicp
(#%require "streams.scm")
(#%require "infinite-streams.scm")
(#%provide (all-defined))

(define (square x) (* x x))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers-implicit (cons-stream 1 (add-streams ones integers)))
; (ss integers)

(define fibs-implicit
  (cons-stream 0
               (cons-stream 1
                            (add-streams
                             (stream-cdr fibs)
                             fibs))))

(define double (scale-stream integers 2))
; (ss double)

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
; the recursive call to primes is in here
(define (prime? x)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) x) true)
          ((divisible? x (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
; (ss primes)