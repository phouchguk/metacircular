(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (enclosing-environment env) (cdr env))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (error "Unknown expression type -- EVAL" env))))

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

(define (make-frame variables values)
  (cons variables values))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define the-empty-environment '())

(define (variable? exp)
  (symbol? exp))

(extend-environment '(a b) '(1 2) '())

(let ((a 1) (b 2)) (+ a b))
