#lang sicp
(define (RLC R L C dt)
  (lambda (vc0 il0)
    ; first, define vc and il, which are necessary for the other
    ; streams, but depend on them
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))

    ; We can use vc and il normally here b/c we handled the delayed evaluation
    ; there
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-stream
                 (scale-stream il (/ (- R) L))
                 (scale-stream vc (/ 1 L))))
    (cons vc il)))