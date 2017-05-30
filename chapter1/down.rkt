#lang racket

(define (down lst)
   (define (brace elem) (cons elem `()))
   (map brace lst))
