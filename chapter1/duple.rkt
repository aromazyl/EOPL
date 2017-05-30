#lang racket

(define (duple n x)
  (cond
        [(eq? n 0) null]
        [else (cons x (duple (- n 1) x))]))
