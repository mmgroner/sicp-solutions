#lang sicp
(define (eval-and exp env)
    (if (null? exp) true
        (if (eval (car exp) env)
            (eval-and (cdr exp) env)
            false)))

(define (eval-or exp env)
    (if null? exp) false
        (if (eval (car exp) env)
            true
            (eval-or (cdr exp) env)))


; In Eval
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

; This would be placed in eval before the `application?` clause

; Data-directed style
(put 'op 'and eval-and)
(put 'op 'or eval-or)