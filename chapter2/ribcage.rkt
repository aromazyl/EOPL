#lang racket


(define empty-env
  (lambda () `()))

(define extend-env
  (lambda (var-list val-list env)
    (list (list var-list val-list) env)))

(define apply-env
  (lambda (env search-var)
    (define (search-sub var-lst val-lst var)
      (cond
       [(null? var-lst) `()]
       [(eqv? (car var-lst) var) (car val-lst)]
       [else (search-sub (cdr var-lst) (cdr val-lst) var)]))
    (cond
      [(null? env) `()]
      [else
       (let ((res (search-sub (car (car env)) (cadr (car env)) search-var)))
         (if (null? res)
             [apply-env (if (null? (cdr env)) null (caddr env)) search-var]
             res))])))

(define extend-env* extend-env)