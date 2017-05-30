#lang racket

(define (g elem lst)
  (cons
   elem
   (map (lambda (x) (list (+ 1 (car x)) (cadr x))) lst)))

(define number-elements
  (lambda (lst)
    (if (null? lst) `()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))