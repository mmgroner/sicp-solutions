#lang sicp
; see http://community.schemewiki.org/?sicp-ex-3.76 for more discussion
; quite a curious bit, these past few exercises

(define (smooth s)
  (cons-stream
   (/ (+ (stream-car s)
         (stream-car (stream-cdr s)))
      2)
   (smooth (stream-cdr s))))

; A more elegant solution of the above
(define (smooth s)
  (stream-map (lambda (x1 x2) (/ (+ x1 x2) 2))
              s
              (stream-cdr s)))

(define (make-zero-crossings sense-data)
  (stream-map sign-cahnge-detector (smooth sense-data) (smooth (stream-cdr sense-data))))
                                                       ; OR
                                                      ;(smooth (cons-stream 0 sense-data)))
                