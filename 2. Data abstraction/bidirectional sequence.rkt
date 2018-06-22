#lang racket

;; 2.18

(define (number->sequence x)
  (list x '() '()))
(define current-element car)
(define (move-to-left s)
  (let* ([current (car s)]
         [new-current (car (second s))]
         [new-left (cdr (second s))]
         [new-right (cons current (third s))])
    (list new-current new-left new-right)))
(define (move-to-right s)
  (let* ([current (car s)]
         [new-current (car (third s))]
         [new-left (cons current (second s))]
         [new-right (cdr (third s))])
    (list new-current new-left new-right)))
(define (insert-to-left new s)
  (match s
    [(list x left right) (list x `(,new . ,left) right)]))
(define (insert-to-right new s)
  (match s
    [(list x left right) (list x left `(,new . ,right))]))