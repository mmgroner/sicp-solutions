#lang sicp

; list-of-values that always takes left-to-right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (car exps) env)
            (list-of-values (cdr exps) env))))

; list-of-values that always takes right-to-left
(define (list-of-values-rtl exps env)
  (list-of-values (reverse list-of-values exps) env))