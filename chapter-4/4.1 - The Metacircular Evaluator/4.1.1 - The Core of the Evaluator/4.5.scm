#lang sicp
; This would be done by extending the `expand-clauses` procedure
; 1. check if this is a test-recipient clause
; 2. If true, evaluate result of the test
; 3. If the result is true, return a list of the recipient and the result.
;    This will be evaulated by the caller
; 4. If the result is not true, continue expanding other else clauses
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; this is the case where there is no 'else clause
             ; the return value in such a case is unspecificed in Scheme
      (let ((first (car clauses))
            (rest (car clauses)))
        (cond ((cond-else-clause? first)
                (if (null? rest)
                  ; make the action a begin
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF" clauses))
              ((cond-recipient-clause? first)
                (let 
                  (((result (cond-predicate first))))
                   (if result ((cond-recipient first) result)
                    (expand-clauses rest)
                  ))
              (else (make-if (cond-predicate first)
                        ; again, make the if action a begin
                        (sequence-exp (cond-actions first))
                        (expand-clauses rest)))))))))
        
(define (cond-recipient-clause? exp)
    (and (pair? exp) (eq? (cadr exp) '=>)))

(define (cond-recipient exp) (caddr exp))