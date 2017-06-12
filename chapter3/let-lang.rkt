#lang eopl

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define identifier? symbol?)

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (plus-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (expr expression?))
  (zero?-exp
   (exp1 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp expression?))
  (cdr-exp
   (exp expression?))
  (null?-exp
   (exp expression?))
  (emptylist-exp
   (exp null?))
  (list-exp
   (exp (list-of expression?)))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?)))


(define init-env
  (lambda ()
    (extend-env
     `i (num-val 1)
     (extend-env
      `v (num-val 5)
      (extend-env
       `x (num-val 10)
       (empty-env))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval-val
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (val) val)
      (else (report-expval-extractor-error `expval val)))))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error `num val)))))

(define report-expval-extractor-error
  (lambda (s val)
    (eopl:error `apply-env "report-expval-extractor-error type: ~s val: ~s" s val)))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error `bool val)))))

;(define run
;  (lambda (string)
;    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define (symbol-op op exp1 exp2 env)
        (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (eval (op num1 num2))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2) (symbol-op `- exp1 exp2 env))
      (plus-exp (exp1 exp2) (symbol-op `+ exp1 exp2 env))
      (mult-exp (exp1 exp2) (symbol-op `* exp1 exp2 env))
      (div-exp (exp1 exp2) (symbol-op `/ exp1 exp2 env))
      (minus-exp (expr) (symbol-op `- (const-exp 0) expr env))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (equal?-exp (exp1 exp2)
                  (eqv?
                   0
                   (expval->num
                    (value-of (diff-exp exp1 exp2) env))))
      (less?-exp (exp1 exp2)
                 (> 0
                    (expval->num
                     (value-of (diff-exp exp1 exp2) env))))
      (greater?-exp (exp1 exp2)
                    (< 0
                       (expval->num
                        (value-of (diff-exp exp1 exp2) env))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (cons-exp (exp1 exp2)
                (cons (expval->num (value-of exp1)) (expval->num (value-of exp2))))
      (car-exp (exp)
               (car (value-of exp)))
      (cdr-exp (exp)
               (cdr (value-of exp)))
      (null?-exp (exp)
                 (expval->bool (value-of exp)))
      (emptylist-exp (exp)
                     `())
      (list-exp (exp)
                (map (lambda (ex) (expval-val value-of ex)) exp))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env)))))))
