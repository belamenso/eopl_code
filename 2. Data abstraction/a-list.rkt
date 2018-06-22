#lang racket

(define (empty-env) '())
(define (extend-env var val env) `((,var . ,val) . ,env))
(define (apply-env env var)
  (cond
    [(empty-env? env) (error (~a "apply-env: No varable name <" var ">"))]
    [(equal? (car (car env)) var) (cdr (car env))]
    [else (apply-env (cdr env) var)]))
(define empty-env? null?)
(define (has-binding? env var)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (let ([v (apply-env env var)])
      #t)))
(define (extend-env* vars vals env)
  (append env (map cons vars vals)))