#lang sicp
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-stream
               ; not sure why we don't use negative signs
               ; none of the answers I found online did
               ; I asked my question on http://community.schemewiki.org/?sicp-ex-3.78 (8/6/24)
               ; maybe someone answered
               (scale-stream dy a)
               (scale-stream y b)))
  y)