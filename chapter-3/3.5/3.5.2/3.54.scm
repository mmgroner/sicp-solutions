#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%provide mul-streams)

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
         (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

; (ss factorials)