#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "exploiting-the-stream-paradigm.scm")
(#%provide merge-weighted)
(#%provide pairs-weighted)

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((weight-s1 (weight s1car))
                 (weight-s2 (weight s2car)))
             (cond ((< weight-s1 weight-s2)
                    (cons-stream
                     s1car
                     (merge-weighted s2 (stream-cdr s1) weight)))
                   ((> weight-s1 weight-s2)
                    (cons-stream
                     s2car
                     (merge-weighted s1 (stream-cdr s2) weight)))
                   (else ; they're equal
                    (cons-stream
                     s1car
                     (cons-stream s2car
                                  (merge-weighted (stream-cdr s1)
                                                  (stream-cdr s2)
                                                  weight))))))))))

(define (pairs-weighted s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (sum-by-weight pair) (+ (car pair) (cadr pair)))
(define int-pairs-by-sum (pairs-weighted integers integers sum-by-weight))
; (ss-map sum-by-weight int-pairs-by-sum 100)

; (ss int-pairs-by-sum 100)
(define (2-3-5-filter pair)
  (define factors (list 2 3 5))
  (define (test p f)
    (if (null? p) true
        (if (null? f)
            (test (cdr p) factors)
            (if (= (remainder (car p) (car f)) 0)
                false
                (test p (cdr f))))))
  (test pair factors))


(define (sum-by-235 pair) (+ (* (car pair) 2)
                             (* (cadr pair) 3)
                             (* (car pair) (cadr pair) 5)))
(define int-pairs-235 (stream-filter
                       2-3-5-filter
                       (pairs-weighted integers integers sum-by-235)))
; (ss-map sum-by-235 int-pairs-235 100)

; (ss int-pairs-2-3-5 100)
