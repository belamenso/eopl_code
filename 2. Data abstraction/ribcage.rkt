#lang racket

(define (empty-env) '())
(define (extend-env var val env) `( ((,var) . (,val)) . ,env))
(define empty-env? null?)
(define (extend-env* vars vals env)
  `((,vars . ,vals) . ,env))
(define (apply-env env var)
  (cond
    [(empty-env? env) (error (~a "apply-env: No varable name <" var ">"))]
    [(let ([found-in-current (let loop ([vars (caar env)]
                                        [vals (cdar env)])
                               (cond
                                 [(null? vars) #f]
                                 [(equal? (car vars) var)
                                  (list (car vals))]
                                 [else (loop (cdr vars) (cdr vals))]))])
       (if found-in-current
           (first found-in-current)
           (apply-env (cdr env) var)))]))

(define (has-binding? env var)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (let ([v (apply-env env var)])
      #t)))

(define e
  (extend-env* '(x y z) '(100 200 300)
               (extend-env 'a '1 '())))


