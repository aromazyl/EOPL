#lang racket

(define (product sos1 sos2)
  (define (zips elem sos)
    (map (lambda x (cons elem x)) sos))
  (cond
    [(null? sos1) null]
    [else (append
            (zips (car sos1) sos2)
            (product (cdr sos1) sos2))]))
