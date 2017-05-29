#lang racket

(define (every? pred lst)
  (foldl (lambda (x y) (and x y)) #t (map pred lst)))

(define (exists? pred lst)
  (foldl (lambda (x y) (or x y)) #f (map pred lst)))