#lang sicp
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable exp env))
        (else (let ((eval-proc ((eval-driver-table 'get) (car exp))))
                (cond (eval-proc (eval-proc exp env))
                      ((application? exp)
                       (apply-process (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
                      (else (error "unkonwn expression! -- EVAL" exp)))))))

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
      false))
(define expression-tag exp-tag)

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (quote? exp)
  (eq? (expression-tag exp) 'quote))

(define (text-of-quote exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (definite? exp)
  (eq? (expression-tag exp) 'define))
(define (definition-variable exp)
  (if (pair? (cadr exp))
      (caadr exp)
      (cadr exp)))
(define (definition-value exp)
  (if (pair? (cadr exp))
      (make-lambda (cdadr exp)
                   (cddr exp))
      (caddr exp)))
(define (make-define var body)
  (cons 'define (cons var body)))

(define (assignment? exp) (eq? (expression-tag exp) 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (if? exp) (eq? (expression-tag exp) 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequence exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (begin? exp) (eq? (expression-tag exp) 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin exps) (cons 'begin exps))

(define (cond? exp) (eq? (expression-tag exp) 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? 'else (car clause)))
(define (cond-apply-clause? clause) (eq? '=> (cadr clause)))
(define (clause-procedure clause) (caddr clause))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (clause-predicate clause) (car clause))
(define (clause-actions clause) (cdr clause))

(define (lambda? exp) (eq? 'lambda (expression-tag exp)))
(define (lambda-params exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (make-procedure body params env)
  (list 'procedure body params env))
(define (procedure-body proc) (cadr proc))
(define (procedure-params proc) (caddr proc))
(define (procedure-env proc) (cadddr proc))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (make-application operator operands) (cons operator operands))

(define (primitive-procedure? exp) (eq? 'primitive-procedure (expression-tag exp)))
(define (make-primitive-procedure proc) (cons 'primitive-procedure proc))
(define (primitive-procedure exp) (cdr exp))
(define (apply-primitive-procedure proc operands)
  (apply (primitive-procedure proc) operands))

(define (compound-procedure? exp) (eq? 'procedure (expression-tag exp)))

(define (let-binds exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (make-let binds body)
  (cons 'let (cons binds body)))
(define (naming-let? exp) (variable? (cadr exp)))
(define (naming-let-variable exp) (cadr exp))
(define (naming-let-binds exp) (caddr exp))
(define (naming-let-body exp) (cdddr exp))
(define (variables-of-binds binds) (map car binds))
(define (values-of-binds binds) (map cadr binds))

(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
;; eval-???
(define (eval-or exp env)
  (eval (or->if exp) env))
(define (or->if exp)
  (define (->if exps)
    (if (null? exps)
        'false
        (make-application
         (make-lambda '(v alternative)
                      (list
                       (make-if 'v
                                'v
                                (make-application 'alternative nil))))
         (list (car exps)
               (make-lambda '()
                            (list (->if (cdr exps))))))))
  (->if (cdr exp)))
          
(define (eval-and exp env)
  (define (iter exps)
    (if (last-pair? exps)
        (eval (car exps) env)
        (let ((e (eval (car exps) env)))
          (if e
              (iter (cdr exps))
              false))))
  (let ((exps (cdr exp)))
    (if (null? exps)
        true
        (iter exps))))
(define (last-pair? exp)
  (and (pair? exp) (null? (cdr exp))))


(define (eval-quote exp env)
  (text-of-quote exp))
(define (eval-lambda exp env)
  (make-procedure (lambda-body exp)
                  (lambda-params exp)
                  env))
(define (eval-cond exp env)
  (eval (cond->if exp) env))
(define (eval-define exp env)
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

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (eval-while exp env)
  (eval (while->combination exp) env))

(define (eval-for exp) nil)
;; derivative syntax
(define (while->combination exp)
  (let ((predicate-exp (while-predicate exp))
        (body-exps (while-body exp)))
    (make-application
     (make-lambda (list 'pred-proc 'body-proc)
                  (list
                   (make-define '(f)
                                (list
                                 (make-if (make-application 'pred-proc
                                                                 nil)
                                               (make-begin (list
                                                            (make-application 'body-proc
                                                                              nil)
                                                            (make-application 'f nil)))
                                               ''done)))
                   (make-application 'f nil)))
     (list (make-lambda '()
                        (list predicate-exp))
           (make-lambda '()
                        body-exps)))))
                                                     
                                          

(define (let*->nested-lets exp)
  (let ((binds (let-binds exp))
        (body (let-body exp)))
    (define (iter binds)
      (if (null? binds)
          (make-let nil body)
          (make-let (list (car binds))
                    (list (iter (cdr binds))))))
    (iter binds)))

(define (let->combination exp)
  (if (naming-let? exp)
      (let ((var (naming-let-variable exp))
            (binds (naming-let-binds exp))
            (body (naming-let-body exp)))
        (make-application
         (make-lambda '()
                      (list (make-define (cons var (variables-of-binds binds))
                                         body)
                            (make-application var
                                              (values-of-binds binds))))
         nil))
      (let ((binds (let-binds exp))
            (body (let-body exp)))
        (make-application
         (make-lambda (map car binds)
                      body)
         (map cadr binds)))))

(define (cond->if exp)
  (clauses->if (cond-clauses exp)))
(define (clauses->if clauses)
  (if (null? clauses)
      false
      (let ((clause (first-clause clauses)))
        (cond ((cond-else-clause? clause)
               (sequence->exp (clause-actions clause)))
              ((cond-apply-clause? clause)
               (make-application
                (make-lambda '(pred consequence alternetive)
                             (list (make-if 'pred
                                            (list (make-application 'consequence nil) 'pred)
                                                  (make-application 'alternetive nil))))
                (list (clause-predicate clause)
                      (make-lambda '()
                                   (list (clause-procedure clause)))
                      (make-lambda '()
                                   (list (clauses->if (rest-clauses clauses)))))))
              (else
               (make-if (clause-predicate clause)
                        (sequence->exp (clause-actions clause))
                        (clauses->if (rest-clauses clauses))))))))


;; env , frame  representation
(define the-empty-env nil)
(define (empty-env? env) (null? env))
(define (make-env frame env) (cons frame env))
(define (first-frame env) (car env))
(define (up-env env) (cdr env))

(define the-empty-frame (list 'frame))
(define (make-empty-frame) (list 'frame))
(define (empty-frame? frame)
  (or (null? frame) (and (eq? (car frame) 'frame) (null? (cdr frame)))))
(define (frame-pairs frame) (cdr frame))
(define (first-pair pairs) (car pairs))
(define (rest-pairs pairs) (cdr pairs))
(define (make-frame keys values)
  (define (iter keys values frame)
    (if (null? keys)
        frame
        (begin (add-frame (car keys) (car values) frame)
               (iter (cdr keys) (cdr values) frame))))
  (iter keys values (make-empty-frame)))
(define (add-frame key val frame)
  (set-cdr! frame
            (cons (cons key val)
                  (frame-pairs frame))))
(define (pair-key pair) (car pair))
(define (pair-value pair) (cdr pair))
(define (set-pair-value! pair new-val) (set-cdr! pair new-val))


(define (scan-environment env todo end)
  (if (empty-env? env)
      (end env)
      (todo env todo end)))
(define (scan-frame frame var todo end)
  (define (iter pre-pairs cur-pairs)
    (if (null? cur-pairs)
        (end)
        (let ((pair (first-pair cur-pairs)))
          (if (eq? (pair-key pair) var)
              (todo pre-pairs cur-pairs)
              (iter (rest-pairs pre-pairs)
                    (rest-pairs cur-pairs))))))
  (if (empty-frame? frame)
      (end)
      (iter frame (frame-pairs frame))))

(define (lookup-variable exp env)
  (scan-environment env
                    (lambda (env self end)
                      (scan-frame (first-frame env)
                                  exp
                                  (lambda (pre-pairs cur-pairs)
                                    (pair-value (first-pair cur-pairs)))
                                  (lambda ()
                                    (scan-environment (up-env env)
                                                      self
                                                      end))))
                    (lambda (env) (error "no found variable -- LOOKUP-VARIABLE" env 'variable: exp))))
(define (extend-env env params args)
  (make-env (make-frame params args)
            env))
(define (definite! var val env)
  (scan-environment env
                    (lambda (env self end)
                      (scan-frame (first-frame env)
                                  var
                                  (lambda (pre-pairs cur-pairs)
                                    (set-pair-value! (first-pair cur-pairs) val))
                                  (lambda ()
                                    (add-frame var val (first-frame env)))))
                    (lambda (env) (error "empty environment -- DEFINITE!" env))))
                    
(define (set-value! var val env)
  (scan-environment env
                    (lambda (env self end)
                      (scan-frame (first-frame env)
                                  var
                                  (lambda (pre-pairs cur-pairs)
                                    (set-pair-value! (first-pair cur-pairs) val))
                                  (lambda ()
                                    (scan-environment (up-env env)
                                                      self
                                                      end))))
                    (lambda (env) (error "empty environment -- SET-VALUE!" env))))

;; initialize global environment,

(define global-environment
  (make-env the-empty-frame the-empty-env))
((lambda ()
   (let ((frame (first-frame global-environment)))
     (define (b-p key val)
       (add-frame key (make-primitive-procedure val) frame))
     (define (b-v key val)
       (add-frame key val frame))

     (b-p 'car car)
     (b-p 'cdr cdr)
     (b-p 'cons cons)
     (b-p '+ +)
     (b-p '- -)
     (b-p '* *)
     (b-p '/ /)
     (b-p '= =)
     (b-p 'apply apply)
     (b-p 'map map)
     (b-p 'null? null?)
     (b-p 'eq? eq?)
     (b-p 'list list)
     (b-p '< <)
     (b-p '> >)
     (b-p 'display display)
     
     (b-v 'false false)
     (b-v 'true true)
     (b-v 'nil nil)
     )))


(define (driver-loop)
  (let ((input (read)))
    (let ((output (eval input global-environment)))
      (user-print output)))
  (driver-loop))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-body object)
                     (procedure-params object)
                     '<procedure-env>))
      (display object))
  (newline))

(define (print o) (display o) (newline))



;; temporary table
(define (make-table)
  (define table '(table))
  (define (empty? table)
    (or (null? table) (and (eq? (car table) 'table) (null? (cdr table)))))
  (define (data table) (car table))
  (define (data-key data) (car data))
  (define (value data) (cdr data))
  (define (set-value! data new-value) (set-cdr! data new-value))
  (define (make-data key value next) (cons (cons key value) next))
  (define (next table) (cdr table))
  (define (set-next! table value) (set-cdr! table value))

  (define (add-table key value)
    (set-next! table (make-data key value (next table))))
  (define (lookup key compare todo notdo)
    (define (iter pre cur)
      (cond ((empty? cur)
             (notdo pre cur))
            ((compare key (data-key (data cur)))
             (todo pre cur))
            (else
             (iter (next pre) (next cur)))))
    (iter table (next table)))
           
  (define (get key)
    (lookup key
            eq?
            (lambda (pre cur)
              (value (data cur)))
            (lambda (pre cur)
              false)))
  (define (set! key new-value)
    (if (empty? table)
        (add-table key new-value)
        (lookup key
                eq?
                (lambda (pre cur)
                  (set-value! (data cur) new-value))
                (lambda (pre cur)
                  (set-next! pre (make-data key new-value (next pre)))))))
  (define (dispatch m)
    (cond ((eq? m 'get) get)
          ((eq? m 'set!) set!)
          ((eq? m 'table) table)
          (else (error "unknown operation! -- MAKE-TABLE" m))))
  dispatch)

;; instrall
(define eval-driver-table (make-table))
((lambda ()
   (define add! (eval-driver-table 'set!))
   (add! 'if eval-if)
   (add! 'cond eval-cond)
   (add! 'lambda eval-lambda)
   (add! 'quote eval-quote)
   (add! 'define eval-define)
   (add! 'set! eval-assignment)
   (add! 'begin eval-begin)
   (add! 'or eval-or)
   (add! 'and eval-and)
   (add! 'let eval-let)
   (add! 'let* eval-let*)
   (add! 'while eval-while)
   (add! 'for eval-for)
   ))