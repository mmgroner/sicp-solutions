#lang racket
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
    (define cond (caddr exp))
    
    (let ()
        (define (loop)
            (proc)
            (if cond proc 'done))
        (loop)))

(define (while->combination exp)
    (define cond (cadr exp))
    (define proc (caddr exp))
    
    (define (loop)
        (if cond (loop) 'done))
    
    (loop))

(define (for->combination ))