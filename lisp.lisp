(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        (else (error "Unknown expression type -- EVAL" env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
