(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-value exp) (caddr exp))

(define (assignment-variable exp) (cadr exp))

(define (caar exp) (car (car exp)))

(define (cadr exp) (car (cdr exp)))

(define (cdar exp) (cdr (car exp)))

(define (cddr exp) (cdr (cdr exp)))

(define (caaar exp) (car (car (car exp))))

(define (caadr exp) (car (car (cdr exp))))

(define (cadar exp) (car (cdr (car exp))))

(define (caddr exp) (car (cdr (cdr exp))))

(define (cdaar exp) (cdr (car (car exp))))

(define (cddar exp) (cdr (cdr (car exp))))

(define (cdddr exp) (cdr (cdr (cdr exp))))

(define (enclosing-environment env) (cdr env))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        (else (error "Unknown expression type -- EVAL" env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
        (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (first-frame env) (car env))

(define (frame-values frame) (cdr frame))

(define (frame-variables frame) (car frame))

(define (length x)
  (define (length-iter x i)
    (if (null? x) i (length-iter (cdr x) (+ 1 i))))
  (length-iter x 0))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (make-frame variables values)
  (cons variables values))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
    false))

(define (text-of-quotation exp) (cadr exp))

(define the-empty-environment '())

(define (variable? exp)
  (symbol? exp))

(extend-environment '(a b) '(1 2) '())

(let ((a 1) (b 2)) (+ a b))
