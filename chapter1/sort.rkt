#lang racket


(define (list-head lst n)
  (cond
    [(= n 0) null]
    [else (cons (car lst) (list-head (cdr lst) (- n 1)))]))


(define (half lst)
  (let ((h (floor (/ (length lst) 2))))
  (list (list-head lst h) (list-tail lst h))))

(define (merge pred lst1 lst2)
  (cond
    [[null? lst1] lst2]
    [[null? lst2] lst1]
    [[pred (car lst2) (car lst1)] (merge pred lst2 lst1)]
    [else (cons (car lst1) (merge pred (cdr lst1) lst2))]))

(define (sort/predicate pred loi)
  (cond
    ((null? loi)
      `())
    ((= 1 (length loi)) loi)
    (else (let ((half-lst (half loi)))
        (merge pred
         (sort/predicate pred (car half-lst))
         (sort/predicate pred (cadr half-lst)))))
    ))

(define (sort loi) (sort/predicate < loi))

  