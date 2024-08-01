#lang sicp
(#%require "../streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%provide (all-defined))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                  (add-streams (scale-stream integrand dt)
                              int)))
  int)
                  