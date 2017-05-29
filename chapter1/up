#lang racket

(define (up lst)
  (cond
    [(null? lst) null]
    [(symbol? lst) (list lst)]
    [else (
           (if (symbol? (car lst))
               cons
               append)
           (car lst) (up (cdr lst)))]))