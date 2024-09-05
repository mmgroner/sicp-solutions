#lang sicp
; see http://community.schemewiki.org/?sicp-ex-3.74 for a little discussion on this one
;
; Although I wrote this function first:
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (stream-cdr sense-data)))
; Upon re-reading the books code,
; particularly this part:
(define zero-crossings
  (make-zero-crossings sense-data 0))
; meteorgan's solution seems to fit better
(define zero-crossings
  (stream-map (sign-change-detector)
              sense-data
              (cons-stream 0 sense-data)))
; Because the initial "last-value" has to be zero

