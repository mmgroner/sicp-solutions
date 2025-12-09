#lang racket

;; Changing the syntax would be as simple as changing the
;; syntax check ans selectors

; eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Uknown expression type: EVAL" exp))))

; quotation
(define (quoted? exp) (tagged-list? exp 'quoted)) ; changed syntax to "quoted"
(define (text-of-quotation exp) (cadr exp))

; assignments
; Let's say I made this the syntax:
; (set# "bar" foo) ; e.g. assign value "bar" to variable foo
(define (assignment exp) (tagged-list? exp 'set#))
(define (assignment-variable exp) (caddr exp))
(define (assignment-value exp) (cadr exp))

; definitions
; a definition could be (define foo bar)
; or
; (define (f a b))
; In this example, well change it to "def"
; Also, I'll change procedure definition to be
; (def f (a b) (+ a b))
(define (definition? exp) (tagged-list? exp 'def))
(define (definition-variable exp) (cadr exp))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      ; (define foo bar)
      (caddr exp)
      ; (define f (a b))
      ; make-lambda is where the syntactic sugar of
      ; (define f (a b) (+ a b))
      ; becomes:
      ; (define f
      ;    (lambda (a b) (+ a b)))
      (make-lambda (caddr exp)    ; formal parameters
                   (cdddr exp))))  ; body

; lambda
; let's make "lmb" be a reserved keyword, for easier typing
(define (lambda? exp) (tagged-list? exp 'lmb))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
; syntactic sugar used in definition-value
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))  

;; above are some examples of what we can do to change the syntax without changing
;; the eval or apply procedures, something which is only possible
;; because we used data abstraction to define the syntax             