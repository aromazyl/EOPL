#lang racket

(define (var-exp var) var)

(define (lambda-exp var lc-exp) (list `lambda (list var) lc-exp))

(define (app-exp lc-exp-a lc-exp-b) (list lc-exp-a lc-exp-b))

(define var-exp? symbol?)

(define (lambda-exp? lc-exp)
  (and
   (= 3 (length lc-exp))
   (eqv? `lambda (car lc-exp))
   (not (symbol? (cadr lc-exp)))
   (= 1 (length (cadr lc-exp)))
   (var-exp? (cadr lc-exp))
   (or
    (var-exp? (caddr lc-exp))
    (lambda-exp? (caddr lc-exp))
    (app-exp? (caddr lc-exp)))))

(define (app-exp? lc-exp)
  (define (sure? exp)
     (or
      (var-exp? exp)
      (lambda-exp? exp)
      (app-exp? exp)))
  (and
   (= 2 (length lc-exp))
   (sure? (car lc-exp))
   (sure? (cadr lc-exp))))

(define (var-exp->var lc-exp)
  (if (var-exp? lc-exp)
      lc-exp
      #f))

(define (lambda-exp->bound-var lc-exp)
  (if (lambda-exp? lc-exp)
      (car (cadr lc-exp))
      #f))

(define (lambda-exp->body lc-exp)
  (if (lambda-exp? lc-exp)
      (caddr lc-exp)
      #f))

(define (app-exp->rator lc-exp)
  (if (app-exp? lc-exp)
      (car lc-exp)
      #f))
(define (app-exp->rand lc-exp)
  (if (app-exp? lc-exp)
      (cadr lc-exp)
      #f))
