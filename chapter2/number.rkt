#lang racket

(define zero `())
(define (successor n) (cons #t n))
(define is-zero? null?)
(define predecessor cdr)
(define (plus n1 n2)
  (cond
    [(is-zero? n1) n2]
    [(is-zero? n2) n1]
    [else (plus (predecessor n1) (successor n2))]))
(define (minus n1 n2)
  (cond
    [(is-zero? n2) n1]
    [else (minus (predecessor n1) (predecessor n2))]))

(define (mult n1 n2)
  (cond
    [(is-zero? n1) zero]
    [(is-zero? n2) zero]
    [(is-zero? (predecessor n1)) n2]
    [(is-zero? (predecessor n2)) n1]
    [else (plus n2 (mult (minus n1 (make-num 1)) n2))]))

(define (make-num n)
  (cond
    [(eq? 0 n) zero]
    [else (successor (make-num (- n 1)))]))

(define (eval n)
  (cond
    [(is-zero? n) 0]
    [else (+ 1 (eval (predecessor n)))]))

(define (factorial n)
  (cond
    [(is-zero? n) (make-num 1)]
    [(equal? (make-num 1) n) (make-num 1)]
    [else (mult n (factorial (minus n (make-num 1))))]))

(define (to-bigit difftree)
  (cond
    [[eq? `(one) difftree] (make-num 1)]
    [else
     (minus
      (to-bigit (cadr difftree))
      (to-bigit (caddr difftree)))]))