; Define a list-of-values that always takes arguments left to right and a version that always takes right to left

; list-of-values uses first-value and rest-of-values to get the arguments. Defining each version is a matter of defining those 2 procedures

; left to right
(define (first-operands exps)
   (car exps))

(define (rest-of-values exps)
   (cdr exps))


; right-to-left
(define (first-operand exps)
  (car (reverse exps))

(define (rest-of-values exps)
  (cdr (reverse exps))