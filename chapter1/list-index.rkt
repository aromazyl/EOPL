#lang racket

(define (list-index pred lst)
  (define (cc pred lst n)
    (cond
      [(null? lst) #f]
      [(pred (car lst)) n]
      [else (cc pred (cdr lst) (+ n 1))]))
  (cc pred lst 0))
