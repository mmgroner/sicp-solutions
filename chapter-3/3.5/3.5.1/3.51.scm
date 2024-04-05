#lang sicp
(#%require "../streams.scm")

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 1)
(stream-ref x 5)
;(stream-ref x 5)

;(stream-ref x 7)