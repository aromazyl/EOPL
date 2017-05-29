#lang racket

(define (flatten lst)
  (cond
    [(null? lst) null]
    [(symbol? lst) (list lst)]
    [else (append (flatten (car lst)) (flatten (cdr lst)))]))
