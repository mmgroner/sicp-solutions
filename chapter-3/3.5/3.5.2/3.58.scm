#lang sicp
(#%require "../streams.scm")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(ss (expand 1 7 10))
(ss (expand 3 8 10))