#lang eopl

; 2.23
(define (identifier? x) (and (not (list? x))
                             (not (equal? x 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

#|
S-list ::= ({S-exp}∗ )
S-exp ::= Symbol | S-list
|#

(define-datatype s-list s-list?
  (empty-list)
  (not-empty-list (head s-exp?)
                  (tail s-list?)))
(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))

; 2.21

(define-datatype env env?
  (empty-env)
  (non-empty-env (var identifier?)
                 (val s-exp?)
                 (old-env env?)))
(define (has-binding? e v)
  (cases env e
    (empty-env () #f)
    (non-empty-env (var val old-env)
                   (if (equal? var v)
                       #t
                       (has-binding? v old-env)))))

; 2.22
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack (top (lambda (x) #t))
                   (rest stack?)))

; 2.24
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (bintree-to-list t)
  (cases bintree t
    (leaf-node (num) `(leaf-node ,num))
    (interior-node (key left right)
                   `(interior-node ,key
                                   ,(bintree-to-list left)
                                   ,(bintree-to-list right)))))

; 2.25
(define (g t)
  ; currentMax should be replaced by any node value in the tree
  (define currentMax -999999) 
  (define currentSymbol #f)
  (define (f t)
    (cases bintree t
      (leaf-node (n) n)
      (interior-node (k l r)
                     (let ([sum (+ (f l) (f r))])
                       (when (currentMax . <= . sum)
                         (set! currentMax sum)
                         (set! currentSymbol k))
                       sum))))
  (f t)
  currentSymbol)

; 2.26
#|
Red-blue-tree    ::= Red-blue-subtree
Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
                 ::= (blue-node {Red-blue-subtree }∗ )
                 ::= (leaf-node Int)
|#
(define (list-of-rb-subtrees? l)
  (and (list? l)
       (let ([l l])
         (if (null? l)
             #t
             (and (rb-subtree? (car l))
                  (list-of-rb-subtrees? (cdr l)))))))
(define-datatype rb-subtree rb-subtree?
  (r-node
   (subtree-1 rb-subtree?)
   (subtree-2 rb-subtree?))
  (b-node
   (subtrees list-of-rb-subtrees?))
  (l-node
   (i integer?)))
(define-datatype rbt rbt?
  (tree
   (t rb-subtree?)))

(define (h t) ; operates on rb-subtrees
  (let build ([depth 0] [t t])
    (cases rb-subtree t
      (r-node (t1 t2) (r-node (build (+ 1 depth) t1)
                              (build (+ 1 depth) t2)))
      (b-node (l) (b-node (let buildList ([l l])
                            (if (null? l)
                                '()
                                (cons (build depth (car l))
                                      (buildList (cdr l)))))))
      (l-node (i) (l-node depth)))))


















