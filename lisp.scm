(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (application? exp) (pair? exp))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (caar exp) (car (car exp)))
(define (cadr exp) (car (cdr exp)))
(define (cdar exp) (cdr (car exp)))
(define (cddr exp) (cdr (cdr exp)))
(define (caaar exp) (car (car (car exp))))
(define (caadr exp) (car (car (cdr exp))))
(define (cadar exp) (car (cdr (car exp))))
(define (caddr exp) (car (cdr (cdr exp))))
(define (cdaar exp) (cdr (car (car exp))))
(define (cdadr exp) (cdr (car (cdr exp))))
(define (cddar exp) (cdr (cdr (car exp))))
(define (cdddr exp) (cdr (cdr (cdr exp))))
(define (caaaar exp) (car (car (car (car exp)))))
(define (caaadr exp) (car (car (car (cdr exp)))))
(define (caadar exp) (car (car (cdr (car exp)))))
(define (caaddr exp) (car (car (cdr (cdr exp)))))
(define (cadaar exp) (car (cdr (car (car exp)))))
(define (cadadr exp) (car (cdr (car (cdr exp)))))
(define (caddar exp) (car (cdr (cdr (car exp)))))
(define (cadddr exp) (car (cdr (cdr (cdr exp)))))
(define (cdaaar exp) (cdr (car (car (car exp)))))
(define (cdaadr exp) (cdr (car (car (cdr exp)))))
(define (cdadar exp) (cdr (car (cdr (car exp)))))
(define (cdaddr exp) (cdr (car (cdr (cdr exp)))))
(define (cddaar exp) (cdr (cdr (car (car exp)))))
(define (cddadr exp) (cdr (cdr (car (cdr exp)))))
(define (cdddar exp) (cdr (cdr (cdr (car exp)))))
(define (cddddr exp) (cdr (cdr (cdr (cdr exp)))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-actions clause) (cdr clause))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (cond-predicate clause) (car clause))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (enclosing-environment env) (cdr env))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application exp?)
         (lisp-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "LISP: Unknown expression type -- EVAL" env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "LISP: ELSE clause isn't last -- COND->IF"
                   clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "LISP: Too many arguments supplied" vars vals)
          (error "LISP: Too few arguments supplied" vars vals))))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial n)
  (fact-iter  1 1 n))

(define (first-exp seq) (car seq))

(define (first-frame env) (car env))

(define (first-operand ops) (car ops))

(define (frame-values frame) (cdr frame))

(define (frame-variables frame) (car frame))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (if-consequent exp) (caddr exp))

(define (if-predicate exp) (cadr exp))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-body exp) (cddr exp))

(define (lambda-parameters exp) (cadr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (length x)
  (define (length-iter x i)
    (if (null? x) i (length-iter (cdr x) (+ 1 i))))
  (length-iter x 0))

(define (lisp-apply procedure arguments)
  (ccons ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
         ((compound-procedure? procedure)
          (eval-sequence
           (procedure-body procedure)
           (extend-environment
            (procedure-parameters procedure)
            arguments
            (procedure-environment procedure))))
         (else
          (error
           "LISP: Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "LISP: Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (make-begin seq) (cons 'begin seq))

(define (make-frame variables values)
  (cons variables values))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (no-operands? ops) (null? ops))

(define (operands exp) (cdr exp))

(define (operator exp) (car exp))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (procedure-parameters p) (cadr p))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (rest-exps seq) (cdr seq))

(define (rest-operands ops) (cdr ops))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "LISP: Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (setup-environment)
  (let ((initial-env
         (extend-environment '() '() the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
    false))

(define (text-of-quotation exp) (cadr exp))

(define the-empty-environment '())

(define the-global-environment (setup-environment))

(define (true? x) (not (not x)))

(define (variable? exp)
  (symbol? exp))
