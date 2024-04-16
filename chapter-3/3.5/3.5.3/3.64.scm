#lang sicp
(#%require "../streams.scm")
(#%require "exploiting-the-stream-paradigm.scm")


(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s1 s0)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))

; (stream-limit (sqrt-stream 2) 0.001)
