#lang racket
(provide do->combination)
; syntax
; (do <proc> <condition>)
; (while <condition> <proc>)
; (for <list> <proc>) where <proc> is a procedre that takes one argument
; (until <condition> <proc>)

; Desugared
; (do <proc> <condition>)
; (let ()
;   (define (loop) 
;       (loop)
;       (if <condition> (loop) 'done)
;   (loop))

; (while <condition> <proc>)
; (let ()
;   (define (loop)
;       (if <condition> (loop) 'done))
;   (loop))

; (for <list> <proc>)
; (let ()
;   (define (loop list)
;      (if (null? list)
;          'done
;         (begin
;           (<proc> (car list))
;           (loop (cdr list)))))
;   (loop list))

; (until <condition> <proc>)
; (let ()
;     (define (loop)
;         (if (not <condition>)
;              <proc>
;               'done))
;     (loop))

(define (do->combination exp)
  (define proc (cadr exp))
  (define pred (caddr exp))
    
  (list 'let 'loop
        (list (list 'proc proc)
              (list 'pred pred))
        '(proc)
        '(if (pred) (loop) 'done)))