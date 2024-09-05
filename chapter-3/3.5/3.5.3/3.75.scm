#lang sicp
; see http://community.schemewiki.org/?sicp-ex-3.75, xdaidliu's answer for a hot take
;
; I like his point, I originally wrote something similar, where I would pass in the before-last-value
; then average them both
;
; When I went on the site, I saw meteorgan's solution of passing the last avpt, which I thought was cleaner
; and more elegant.
;
; But in concept, I resonate with what xdavidliu is saying
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                 last-value)
                 2)))
  (cons-stream
   (sign-change-detector avpt last-avpt)
   (make-zero-crossings
    (stream-cdr input-stream) (stream-car input-stream) avpt))))
    
                 