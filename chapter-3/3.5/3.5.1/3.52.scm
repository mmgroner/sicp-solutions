#lang sicp
(#%require "../streams.scm")
;(define (show-sum sum))
  ;(display "sum is: ") (display sum) (newline))

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
; sum is 1
;(show-sum sum)
(define y (stream-filter even? seq))
; sum is 6
;(show-sum sum)
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
; sum is 10
;(show-sum sum)
(stream-ref y 7)
; sum is 136
;(show-sum sum)
(display-stream z)
; sum is 210
;(show-sum sum)