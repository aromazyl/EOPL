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

(define (double-tree bintree)
  (cond
    [(leaf? bintree) (leaf (* 2 (content-of bintree)))]
    [else
     (interior-node (* 2 (content-of bintree)))
     (lson bintree) (rson bintree)]))

(define (mark-leaves-with-red-depth bintree)
  (define (rd tree depth)
    (cond
      [(leaf? tree) (leaf depth)]
      [(eq? `red (content-of tree))
       (interior-node
        (content-of tree)
        (rd (lson tree) (+ 1 depth))
        (rd (rson tree) (+ 1 depth)))]
      [else (interior-node
             (content-of tree)
             (rd (lson tree) depth)
             (rd (rson tree) depth))]))
  (rd bintree 0))

(mark-leaves-with-red-depth
 (interior-node `red
                (interior-node `bar
                               (leaf 26)
                               (leaf 12))
                (interior-node `red
                               (leaf 11)
                               (interior-node `quux
                                              (leaf 117)
                                              (leaf 14)))))