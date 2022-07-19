#lang sicp
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quote? exp) (text-of-quote exp))
        ((variable? exp) (lookup-variable exp env))
        ((definite? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((if? exp) (eval-if exp env))
        ((begin? exp) (eval-begin exp env))
        ((cond? exp) (eval (cond->if exp env) env))
        ((lambda? exp) (make-procedure (lambda-body exp)
                                       (lambda-params exp)
                                       env))
        ((application? exp)
         (apply-process (eval (operator exp) env)
                (value-of-operands exp env)))
        (else (error "unkonwn expression! -- EVAL" exp))))

(define (apply-process proc operands)
  (if (primitive-procedure? proc)
      (apply-primitive-procedure proc operands)
      (if (compound-procedure? proc)
          (eval-sequence (procedure-body proc)
                         (extend-env (procedure-env proc)
                                     (procedure-params proc)
                                     operands))
          (error "unkonwn proc! -- APPLY-PROCESS" proc))))

;; syntax
(define (exp-tag exp)
  (if (pair? exp)
      (car exp)
      (error "unkown expression -- EXP-TAG" exp)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (quote? exp)
  (eq? (expression-tag exp) 'quote))

(define (text-of-quote exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (definite? exp)
  (eq? (expression-tag exp) 'define))
(define (definition-variable exp) (cadr exp))
(define (definition-value exp) (caddr exp))

(define (assignment? exp) (eq? (expression-tag exp) 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (if? exp) (eq? (expression-tag exp) 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequence exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (make-if predicate consequence alternative)
  (list predicate consequence alternative))

(define (begin? exp) (eq? (expression-tag exp) 'begin))
(define (begin-actions exp) (cdr exp))

(define (cond? exp) (eq? (expression-tag exp) 'cond))
(define (cond-clauses exp) (cdr exp))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (clause-predicate clause) (car clause))
(define (clause-actions clause) (cdr clause))
;; eval-???
(define (eval-definition exp env)
  (definite! (definition-variable exp)
             (eval (definition-value exp) env)
             env))
(define (eval-assignment exp env)
  (set-value! (assignment-variable exp)
              (eval (assignment-value exp) env)
              env))
(define (eval-if exp env)
  (if (eval (if-predicate exp) env)
      (eval (if-consequence exp) env)
      (eval (if-alternative exp) env)))

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

;; derivative syntax
(define (cond->if exp)
  (clauses->if (cond-clauses exp)))
(define (clauses->if clauses)
  (if (null? clauses)
      false
      (let ((clause (first-clause clauses)))
        (if (cond-else-clause? clause)
            (sequence->exp (clause-actions clause))
            (make-if (clause-predicate clause)
                     (sequence->exp (clause-actions clause))
                     (clauses->if (rest-clauses clauses)))))))


;; env  representation
(define (lookup-variable exp env) )