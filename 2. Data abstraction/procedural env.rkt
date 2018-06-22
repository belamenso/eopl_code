#lang racket

(define (empty-env)
  (list
   (λ (x) ; get data
     (error (~a "No binding named <" x ">")))
   (λ () ; is empty?
     #t)))

(define (extend-env var val env)
  (list
   (λ (x)
     (if (equal? x var)
         val
         ((first env) x)))
   (λ () #f)))

(define (apply-env env var) ((first env) var))

(define (empty-env? env) ((second env)))

(define (has-binding? env x)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (let ([v (apply-env env x)])
      #t)))