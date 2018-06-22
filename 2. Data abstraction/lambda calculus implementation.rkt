#lang racket

(define (var-exp var)
  (cons
   (λ () 'var-exp)
   (λ () var)))
(define (var-exp? x) (equal? 'var-exp ((car x))))
(define (var-exp->var ve) ((cdr ve)))

(define (λ-exp var exp)
  (cons
   (λ () 'λ-exp)
   (λ (selector) (selector (list var exp)))))
(define (λ-exp? x) (equal? ((car x)) 'λ-exp))
(define (λ-exp->bound-var λe) ((cdr λe) first))
(define (λ-exp->body λe) ((cdr λe) second))

(define (app-exp rator rand)
  (cons
   (λ () 'app-exp)
   (λ (selector) (selector (list rator rand)))))
(define (app-exp? x) (equal? ((car x)) 'app-exp))
(define (app-exp->rator ae) ((cdr ae) first))
(define (app-exp->rand ae) ((cdr ae) second))

(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((λ-exp? exp)
       (and
        (not (eqv? search-var (λ-exp->bound-var exp)))
        (occurs-free? search-var (λ-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))

#;(occurs-free? 'a (λ-exp (var-exp 'a) (var-exp 'b)))
