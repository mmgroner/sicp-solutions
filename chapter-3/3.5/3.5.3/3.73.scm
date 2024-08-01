#lang sicp
(#%require "../streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "integral.scm")
; 3.73

(define (RC r c dt)
  (lambda (i v) ; i -> input, v -> intial voltage
    (cons-stream initial-voltage
                 (add-streams
                  (scale-stream i r)
                  (integral
                   (scale-stream i (/ 1 c))
                   v
                   dt)))))

(define RC1 (RC 5 1 0.5))