#lang sicp
(#%require "../streams.scm")
(#%require "exploiting-the-stream-paradigm.scm")

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(ss ln2-stream)

(ss (accelerated-sequence euler-transform ln2-stream))