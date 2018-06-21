#lang racket

(require srfi/43)

(provide map*)

(define atom? (not/c list?))

(define/contract (in-S? n)
  (integer? . -> . boolean?)
  (and (not (negative? n))
       (or
        (zero? n)
        (in-S? (- n 3)))))

(define (list-length l)
  (match l
    ['()  0]
    [(cons _ l) (add1 (list-length l))]))

(define (nth-element n l)
  (if (zero? n)
      (car l)
      (nth-element (sub1 n) (cdr l))))

(define (remove-first s l)
  (cond
    [(null? l) '()]
    [(equal? s (car l)) (cdr l)]
    [else (cons (car l) (remove-first s (cdr l)))]))

(define (remove s l)
  (cond
    [(null? l) '()]
    [(equal? s (car l)) (remove s (cdr l))]
    [else (cons (car l) (remove s (cdr l)))]))

(define (occurs-free? s exp)
  (match exp
    [(list x y) (or
                 (occurs-free? s x)
                 (occurs-free? s y))]
    [(list λ (list var) exp) (and
                              (not (equal? var s))
                              (occurs-free? s exp))]
    [var (equal? s var)]))

(define (subst new old l)
  (let loop ([l l])
    (cond
      [(null? l) '()]
      [(atom? (car l)) (if (equal? (car l) old)
                           (cons new (loop (cdr l)))
                           (cons (car l) (loop (cdr l))))]
      [else (cons (loop (car l))
                  (loop (cdr l)))])))

(define (map* f l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (cons (f (car l)) (map* f (cdr l)))]
    [else (cons (map* f (car l))
                (map* f (cdr l)))]))

(define (subst/ new old l)
  (map*
   (λ (x) (if (equal? x old) new x))
   l))

(define (number-elements l)
  (let loop ([c 0] [l l])
    (if (null? l)
        '()
        (cons `(,c . ,(car l))
              (loop (add1 c) (cdr l))))))

(define v (list->vector (range 1 11)))

(define (sum-vector v)
  (let loop ([i 0])
    (if (= (vector-length v) i)
        0
        (+ (vector-ref v i)
           (loop (add1 i))))))

(define (sum-vector/ v) (foldl + 0 (vector->list v)))
