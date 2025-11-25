#lang sicp
(define (eval exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((get 'operation (operator exp) ((get 'operation (operator exp) exp env))))
    ((application? exp) (apply (eval operator exp) env) (list-of-values (operands exp) env))
    (else (error "Unknown expression type: EVAL" exp))))

(put 'operation 'quote text-of-quotation)
(put 'operation 'begin eval-sequence)
; and so on for all the other operations

; First, we check to see if the expression is self-evaluating or a variable. We cannot use
; data-directed dispatching for such values, because they are not messages to be passed to
; a dispatch method, nor do they have values on the table that can be used as operations.

; Then, we check to see if the expression has a pre-defined operation on the table.
; If yes, we apply the operation to the expression with it's environment.
; If not, then we check to see if this is a procedure application.
; > NOTE: we could have included procedure applications in the table 
; > by requiring the keyword `call` to be used whenever wanting to apply a procedure.
; If this is not an application, then we throw an error.