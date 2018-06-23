#lang eopl

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
(define (report-invalid-concrete-syntax x)
  (eopl:error "Problem"))

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var)
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list
                (unparse-lc-exp rator) (unparse-lc-exp rand))))))

; 2.28

(define (unparse/ exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'proc bound-var '=>
                        (unparse/ body)))
      (app-exp (rator rand)
               (list
                (unparse/ rator) (list (unparse/ rand))))))

; 2.30 finish it
(define (parse-expression datum)
    (cond
      ((symbol? datum) (if (eq? 'lambda datum)
                           (eopl:error "lambda cannot be used as an identifier")
                           (var-exp datum)))
      ((pair? datum)
       (let ([len (length datum)])
         (if (not (or (= len 2) (= len 3)))
             (eopl:error "bad number of elements")
             (if (eqv? (car datum) 'lambda)
                 (lambda-exp
                  (car (cadr datum))
                  (parse-expression (caddr datum)))
                 (app-exp
                  (parse-expression (car datum))
                  (parse-expression (cadr datum)))))))
       (else (report-invalid-concrete-syntax datum))))

(define e (parse-expression '(lambda (x) (f g))))

; 2.31
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define k '(- - 3 2 - 4 - 12 7))

(define (parse-rpn l)
  ; returns first parsed (number or expression) and the remaining list
  ; in a pair because no let-values in eopl lang
  (cond
    [(null? l) (eopl:error "Cannot parse an empty list")]
    [(eq? '- (car l))
     (let* ([r1 (parse-rpn (cdr l))]
            [exp1 (car r1)]
            [l (cdr r1)]
            [r2 (parse-rpn l)]
            [exp2 (car r2)]
            [l (cdr r2)])
       (cons (diff-exp exp1 exp2)
             l))]
    [else (cons (const-exp (car l))
                (cdr l))]))