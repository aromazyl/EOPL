#lang racket

(require "../eopl/eopl.rkt")

(define empty-env
  (lambda () `()))

(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

(define apply-env
  (lambda (env search-var)
    (cond
      [(null? env) (report-no-binding-found search-var)]
      [else (let ((saved-var (car (car env)))
             (saved-val (cadr (car env)))
             (saved-env (if (null? (cdr env)) null (cadr env))))
         (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))])))

(define empty-env? null?)

(define (has-binding? env s)
  (cond
    [(empty-env? env) #f]
    [else (let ((saved-var (car (car env)))
             (saved-val (cadr (car env)))
             (saved-env (if (null? (cdr env)) null (cadr env))))
         (if (eqv? s saved-var)
             #t
           (has-binding? saved-env s)))]))

(define (extend-env* var-list val-list env)
  (cond
    [(null? var-list) env]
    [else
     (extend-env*
      (cdr var-list) (cdr val-list)
      (extend-env (car var-list) (car val-list) env))]))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error `apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error `apply-env "Bad environment: ~s" env)))
