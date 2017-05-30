#lang racket

(define (invert lst)
  (cond
    [(null? lst) '()]
    [else (cons (list (cadr (car lst)) (car (car lst)))
          (invert (cdr lst)))]))
