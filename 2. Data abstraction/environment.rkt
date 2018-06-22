#lang racket

(define (empty-env) (list 'empty-env))

(define (extend-env var val env)
  (list 'extended-env var val env))

(define (apply-env env search-var)
  (cond
    [(equal? (car env) 'empty-env)
     (report-no-binding-found search-var)]
    [(equal? (car env) 'extended-env)
     (let ([saved-var (second env)]
           [saved-val (third env)]
           [saved-env (fourth env)])
       (if (equal? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))]
    [else (report-invalid-env env)]))

(define (report-no-binding-found search-var)
  (raise (~a "apply-env: No bindings for variable name <" search-var ">")))

(define (report-invalid-env env)
  (raise (~a "apply-env: Invalid environent " env)))

(apply-env (extend-env 'b 101 (extend-env 'a 122 (empty-env))) '|a huj panu w dupę|)