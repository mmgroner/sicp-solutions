#lang sicp
; simple test func
(define (loop-lambda)
  (display "LOOP")
  (newline))

(define (eval-pred pred)
  (if (procedure? pred)
      (pred)
      pred))

; do
(define (do proc pred)
  (define (loop)
    (proc)
    (if (eval-pred pred) (loop) 'done))
  (loop))

(display "DO false")
(do loop-lambda #f)

(display "DO false as lambda")
(do loop-lambda (lambda () #f))

(display "DO until 10")
(define i 0)
(do (lambda ()
      (loop-lambda)
      (display "i: ")
      (display i)
      (newline)
      (set! i (+ i 1)))
  (lambda ()
    (< i 10)))

(define (while pred proc)
  (define (loop)
    (if (eval-pred pred)
        (begin
          (proc)
          (loop))
        'done))
  (loop))

(display "WHILE false")
(while #f loop-lambda)

(display "WHILE false as lambda")
(while (lambda () #f) loop-lambda)

(display "WHILE i < 10")
(set! i 0)
(while (lambda () (< i 10))
       (lambda ()
         (display "i: ")
         (display i)
         (newline)
         (set! i (+ i 1))))

; for
(define (for list proc)
  (define (loop list)
    (if (null? list)
        'done
        (begin
          (proc (car list))
          (loop (cdr list)))))
  (loop list))

(define l (list 1 2 3 4 5))
(for l (lambda (e) (display e) (newline)))

; until
(define (until pred proc)
  (define (loop)
    (if (eval-pred pred)
        'done
        (begin
          (proc)
          (loop))))
  (loop))

(display "UNTIL true")
(until #t loop-lambda)

(display "UNTIL 10")
(set! i 0)
(until (lambda () (> i 10))
       (lambda ()
         (display "i: ")
         (display i)
         (newline)
         (set! i (+ i 1))))
