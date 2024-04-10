#lang sicp
(#%require "../streams.scm")
(#%require "../defining-streams-implicitly.scm")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

; (ss (partial-sums integers))