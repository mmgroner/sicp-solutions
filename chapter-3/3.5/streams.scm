#lang sicp
(#%provide (all-defined))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map-simple proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map-simple proc (stream-cdr s)))))

; from 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (if (stream-null? s)
      the-empty-stream
      (if (pred (stream-car s))
          (cons-stream (stream-car s) (stream-filter pred (stream-cdr s)))
          (stream-filter pred (stream-cdr s)))))


; infinite loop alert!
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

; show stream
(define (ss stream . limit)
  (let ((end (if (not (null? limit)) (car limit) 10)))
    (define (step s n)
      (cond ((stream-null? s) (newline))
            ((< n end) (begin
                         (display (stream-car s)) (newline)
                         (step (stream-cdr s) (+ n 1))))
            (else '...)))
    (step stream 1)))

(define (ss-map proc s limit)
  (if (or (stream-null? s) (= limit 0))
      the-empty-stream
      (cons (proc (stream-car s)) (ss-map proc (stream-cdr s) (- limit 1)))))

; some common streams to play with
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (stream-map + ones integers)))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (scale-stream s factor) (stream-map (lambda (x) (* x factor)) s))

(define (partial-sums s)
  (cons-stream (stream-car s) (stream-map + (partial-sums s) (stream-cdr s))))

(define (stream->list s n)
  (if (or (stream-null? s) (= n 0))
      '()
      (cons (stream-car s) (stream->list (stream-cdr s) (- n 1)))))