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
                (list-of-values (operands exp) env)))
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
(define expression-tag exp-tag)

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
(define (make-begin exps) (cons 'begin exps))

(define (cond? exp) (eq? (expression-tag exp) 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? 'else (car clause)))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (clause-predicate clause) (car clause))
(define (clause-actions clause) (cdr clause))

(define (lambda? exp) (eq? 'lambda (expression-tag exp)))
(define (lambda-params exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-procedure body params env)
  (list 'procedure body params env))
(define (procedure-body proc) (cadr proc))
(define (procedure-params proc) (caddr proc))
(define (procedure-env proc) (cadddr proc))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (primitive-procedure? exp) 1)
(define (apply-primitive-procedure proc operands) 1)

(define (compound-procedure? exp) (eq? 'procedure (expression-tag exp)))
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

(define (list-of-values exps env)
  (if (null? exps)
      nil
      (let ((left (eval (car exps) env)))
        (cons left
              (list-of-values (cdr exps) env)))))

(define (eval-sequence exps env)
  (cond ((null? (cdr exps)) (eval (car exps) env))
        (else (eval (car exps) env)
              (eval-sequence (cdr exps) env))))

(define (sequence->exp se)
  (make-begin se))
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
(define the-empty-env nil)
(define (empty-env? env) (null? env))
(define (frame-to-env frame env) (cons frame env))
(define (first-frame env) (car env))
(define (up-env env) (cdr env))

(define the-empty-frame (list 'frame))
(define (empty-frame? frame)
  (or (null? frame) (and (eq? (car frame) 'frame) (null? (cdr frame)))))
(define (frame-pairs frame) (cdr frame))
(define (first-pair pairs) (car pairs))
(define (rest-pairs pairs) (cdr pairs))
(define (add-frame key val frame)
  (set-cdr! frame
            (cons (cons key val)
                  (frame-pairs frame))))
(define (pair-key pair) (car pair))
(define (pair-value pair) (cdr pair))
(define (set-pair-value! pair new-val) (set-cdr! pair new-val))

(define (lookup-variable exp env) 1)
(define (extend-env env params args) 1)
(define (definite! var val env) 1)
(define (set-value! var val env) 1)