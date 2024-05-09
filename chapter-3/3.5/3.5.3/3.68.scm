#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "exploiting-the-stream-paradigm.scm")

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

(define int-pairs (pairs integers integers))

;; don't run! this will run infinitely