#lang racket

; first naturals representation
(define (z1) '())
(define (s1 n) (cons #t n))
(define (p1 n) (cdr n))
(define z1? null?)

; scheme number representation
; 0 zero? add1 sub1

; 2.1 bignum
(define N 1000)
(define (z2) '())
(define z2? null?)
(define (s2 b)
  (let loop ([b b])
    (cond
      [(null? b) '(1)]
      [(= (sub1 N) (car b)) (cons 0 (loop (cdr b)))]
      [else (cons (add1 (car b)) (cdr b))])))
(define (p2 b) ; b != z2
  (let ([remove-trailing-zeros
         (λ (l)
           (reverse 
            (let loop ([l (reverse l)])
                       (cond
                         [(null? l) '()]
                         [(zero? (car l)) (loop (cdr l))]
                         [else l]))))]
        [result
         (let ([h (car b)] [t (cdr b)])
           (if (zero? h)
               (cons (sub1 N) (p2 t))
               (cons (sub1 h) t)))])
    (remove-trailing-zeros result)))
(define (bignum-+ a b)
  (if (z2? a)
      b
      (s2 (bignum-+ (p2 a) b))))
(define (bignum-* a b)
  (if (z2? a)
      (z2)
      (bignum-+ b (bignum-* (p2 a) b))))
(define (bignum-fact n)
  (if (null? n)
      '(1)
      (bignum-* n (bignum-fact (p2 n)))))
#;(bignum-fact '(10))

; 2.3 diff
(define diff cons)
(define (diff->num d)
  (if (not (pair? d))
      d
      (- (diff->num (car d)) (diff->num (cdr d)))))
(define (diff-zero? d) (zero? (diff->num d)))
(define (diff-add1 d) (diff d (diff (diff 1 1) 1)))
(define (diff-sub1 d) (diff d 1))
(define diff-sub diff)
(define (diff-add a b) (diff a (diff (diff 1 1) b)))

(define one (diff 1 (diff 1 1)))
(define e diff->num)
(define two (diff-add1 one))
(define three (diff-add1 (diff-add1 one)))
(e two)
(e three)
(e (diff-add three two))