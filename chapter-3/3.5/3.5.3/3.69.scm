#lang sicp
(#%require "../streams.scm")
(#%require "../infinite-streams.scm")
(#%require "../defining-streams-implicitly.scm")
(#%require "exploiting-the-stream-paradigm.scm")

(define (triples s t u)
  (cons-stream
   (map stream-car (list s t u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))
                ; we use stream-cdr of t and u to avoid duplicating
                ; the list which contains the first elm of each stream,
                ; which has already been explicitly created above
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define int-triples (triples integers integers integers))

(define pythagorean-triples
  (stream-map
   (lambda (triple)
     (begin
       (display (list (car triple) '^2 (square (car triple)) '+ (cadr triple) '^2 (square (cadr triple)) '= (caddr triple) '^2 (square (caddr triple))))
       (newline)
       triple))
   (stream-filter
    (lambda (triple)
      (= (+ (square (car triple)) (square (cadr triple))) (square (caddr triple))))
    int-triples)))

(ss pythagorean-triples 6)
