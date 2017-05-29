#lang racket

(define leaf
  (lambda (content) content))

(define interior-node
  (lambda (content lnode rnode)
    (list content lnode rnode)))

(define (leaf? bintree)
  (not (pair? bintree)))

(define lson cadr)

(define rson caddr)

(define (content-of bintree)
  (if (leaf? bintree)
      (bintree)
      (car bintree)))
