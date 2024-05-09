#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "exploiting-the-stream-paradigm.scm")

; alternate implementation of pairs from the one provided in "exploiting-the-stream-paradigm"
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t))) ; the extra stream
                 (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))

(define int-pairs (pairs integers integers))

(ss int-pairs 100)