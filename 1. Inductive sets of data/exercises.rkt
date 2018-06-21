#lang racket

(require "./chapter.rkt")

(define (duple n s)
  (if (zero? n)
      '()
      (cons s (duple (sub1 n) s))))

(define (invert l)
  (map
   (λ (p) (list (second p) (first p)))
   l))

(define (down l)
  (map
   (λ (x) (cons x '()))
   l))

(define (swapper s1 s2 l)
  (map*
   (λ (x) (cond
            [(equal? x s1) s2]
            [(equal? x s2) s1]
            [else x]))
   l))

(define (list-set l n x)
  (let loop ([i 0] [l l])
    (if (null? l)
        '()
        (cons (if (equal? i n)
                  x
                  (car l))
              (loop (add1 i) (cdr l))))))

(define (product l1 l2)
  (define (prefix x l) (map (λ (e) (list x e)) l))
  (if (null? l1)
      '()
      (append
       (prefix (car l1) l2)
       (product (cdr l1) l2))))

(define (up l) (map (λ (x) (car x)) l))

(define (flatten l)
  (cond
    [(null? l) '()]
    [(list? (car l)) (append (flatten (car l))
                             (flatten (cdr l)))]
    [else (cons (car l) (flatten (cdr l)))]))

(define (merge l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (let ([a (car l1)] [b (car l2)])
            (if (a . <= . b)
                (cons a (merge (cdr l1) l2))
                (cons b (merge l1 (cdr l2)))))]))

(define (sort pred l)
  (if (null? l)
      '()
      (let* ([x (car l)]
             [smaller (filter (λ (e) (pred e x)) (cdr l))]
             [bigger (filter (λ (e) (not (pred e x))) (cdr l))])
        (append (sort pred smaller)
                (list x)
                (sort pred bigger)))))

;; trees

(define (leaf x) x)
(define leaf? (not/c list?))
(define (tree x t1 t2) (list x t1 t2))
(define lson second)
(define rson third)
(define (contents-of x) (if (leaf? x)
                            x
                            (car x)))

(define (double-tree t)
  (if (leaf? t)
      (* 2 t)
      (tree (contents-of t)
            (double-tree (lson t))
            (double-tree (rson t)))))

#;(double-tree (tree 'h (tree 'e 1 2) 3))

(define (mark-tree-with-depth t)
  (let traverse ([depth 0] [t t])
    (if (leaf? t)
        (leaf depth)
        (let* ([c (contents-of t)]
               [new-depth (if (equal? c 'red)
                             (add1 depth)
                             depth)])
          (tree c
                (traverse new-depth (lson t))
                (traverse new-depth (rson t)))))))

;; 1.34

(define (path x t) ; binary tree, book proposed binary search tree
  (if (leaf? t)
      (if (equal? x t) '() #f)
      (if (equal? (contents-of t) x)
          '()
          (let ([left-search (path x (lson t))])
            (if left-search
                (cons 'left left-search)
                (let ([right-search (path x (rson t))])
                  (if right-search
                      (cons 'right right-search)
                      #f)))))))

#;(define t '(1 (2 3
                 4)
              (5 6
                 7)))
#;(path 3 t)

;; 1.35

(define (number-leaves t) ; depth-first, state
  (define c -1)
  (let f ([t t])
    (if (leaf? t)
        (begin
          (set! c (add1 c))
          c)
        (tree (contents-of t)
              (f (lson t))
              (f (rson t))))))

(define (number-leaves/ t); proper one, inner function returns multiple values
  (define (f t c)
    (if (leaf? t)
        (values c (add1 c))
        (let-values ([(left-result c) (f (lson t) c)])
          (let-values ([(right-result c) (f (rson t) c)])
            (values
             (tree (contents-of t)
                   left-result
                   right-result)
             c)))))
  (let-values ([(result c) (f 0)])
    result))

#;(define t '(a (b 3
                 (c 12
                    19))
              (c 6
                 7)))
#;(number-leaves t)

;;; 1.36

(define (g head rest)
  (if (null? rest)
      (cons head rest)
      (cons head (map
                  (λ (p) (cons (add1 (car p)) (cdr p)))
                  rest))))

(define (number-elements l)
    (if (null? l)
        '()
        (g (list 0 (car l))
           (number-elements (cdr l)))))

#;(number-elements '(a b c d e))
