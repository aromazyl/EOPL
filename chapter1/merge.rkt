#lang racket

(define (merge s1 s2)
  (cond
    [(null? s1) s2]
    [(null? s2) s1]
    [(> (car s1) (car s2)) (merge s2 s1)]
    [(cons (car s1) (merge (cdr s1) s2))]))
  
