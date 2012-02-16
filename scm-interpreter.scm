#lang r5rs

; SICP, chapter 4.

(define true #t)
(define false #f)

(define (error str . l)
  (display (cons str l))
  (newline))

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((let? exp) (my-eval (let->combination exp) env))
        ((let*? exp) (my-eval (let*->nested-lets exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp)
         (my-eval (cond->if exp) env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "EVAL -- Unknown type of the expression" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "APPLY -- Unknown type of the procedure" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-operands exp)
  (cdr exp))

(define (eval-and exp env)
  (define (iter seq)
    (if (null? seq)
        true
        (if (my-eval (first-exp seq) env)
            (iter (rest-exps seq))
            false)))
  (iter (and-operands exp)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-operands exp)
  (cdr exp))

(define (eval-or exp env)
  (define (iter seq)
    (if (null? seq)
        false
        (if (my-eval (first-exp seq) env)
            true
            (iter (rest-exps seq)))))
  (iter (or-operands exp)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

(define (list-of-values-ltr exps env)
  (cond ((no-operands? exps) 
         '())
        (else 
         (let ((t (my-eval (first-operand exps) env)))
           (cons t
                 (list-of-values-ltr (rest-operands exps) env))))))

(define (list-of-values-rtl exps env)
  (cond ((no-operands? exps)
         '())
        (else 
         (let ((t (list-of-values-rtl exps env)))
           (cons (my-eval (first-operand exps) env)
                 t)))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? l tag)
  (if (list? l)
      (eq? (car l) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (list? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (make-application fun args)
  (list fun args))

(define (cond-recipience-clause? exp)
  (eq? (cadr exp) '=>))

(define (cond-recipience-fun exp)
  (caddr exp))

(define (expand-clauses clauses)
  (if (null? clauses)
      false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "The else clause is not the last one."
                          clauses)))
              ((cond-recipience-clause? first)
               (make-if (cond-predicate first)
                        (make-application (cond-recipience-fun first)
                                          (cond-predicate first))
                        (expand-clauses rest)))
              (else 
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (named-let? exp)
  (and (tagged-list? exp 'let) (variable? (cadr exp))))

(define (let-named-binds exp)
  (caddr exp))

(define (let-named-body exp)
  (cdddr exp))

(define (let-named-fname exp)
  (cadr exp))

(define (make-let binds body)
  (list 'let binds body))

(define (let-binds exp)
  (cadr exp))

(define (first-bind binds)
  (car binds))

(define (rest-binds binds)
  (cdr binds))

(define (last-bind? binds)
  (null? (cdr binds)))

(define (make-bind var exp)
  (list var exp))

(define (bind-var bind) 
  (car bind))

(define (bind-exp bind) 
  (cadr bind))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (let* ((vars (map bind-var (let-named-binds exp)))
             (exps (map bind-exp (let-named-binds exp)))
             (fname (let-named-fname exp))
             (fun (make-lambda vars (let-named-body exp))))
        (make-let (list (make-bind fname fun))
                  (make-application fname exps)))
      (let ((vars (map bind-var (let-binds exp)))
            (exps (map bind-exp (let-binds exp))))
        (make-application 
         (make-lambda vars (let-body exp))
         exps))))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (helper binds)
    (if (last-bind? binds)
        (make-let (first-bind binds)
                  (let-body exp))
        (make-let (first-bind binds)
                  (helper (rest-binds binds)))))
  (helper (let-binds exp)))

; Chapter 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame vars vals)
  (cons vars vals))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "EXTEND-ENVIRONMENT -- the number of variables doesn't match with the number of values")))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "LOOKUP-VARIABLE-VALUE -- unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) 
             (env-loop (enclosing-environment env)))
            ((eq? (car vars) var)
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "SET-VARIABLE-VALUE! -- unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? (car vars) var)
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define input-prompt "Scheme> ")

(define output-prompt "Res: ")

(define (repl)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (repl))

(define (announce-output s)
  (display s))

(define (prompt-for-input s)
  (newline) (display s))

(define (compound-procedure? object)
  (tagged-list? object 'procedure))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(repl)