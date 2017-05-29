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

(define (path num bst)
  (define (m-path stk m-bst num)
    (cond
      [(leaf? m-bst) `(#f)]
      [(eq? num (content-of m-bst)) (cons #t stk)]
      [(< num (content-of m-bst)) (m-path (cons `left stk) (lson m-bst) num)]
      [else (m-path (cons `right stk) (rson m-bst) num)]))
  (reverse
   (let ((res (m-path `() bst num)))
     (cond
       [(car res) (cdr res)]
       [else `()]))))

(path 17 `(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))

(define (number-leaves tree)
  (define index 0)
  (define (nu-lea tre)
  (cond
    [(leaf? tre)
     (set! index (+ index 1))
     (leaf (- index 1))]
    [else (interior-node
           (content-of tre)
           (nu-lea (lson tre))
           (nu-lea (rson tre)))]))
  (nu-lea tree))

(number-leaves
 (interior-node `foo
                (interior-node `bar
                               (leaf 26)
                               (leaf 12))
                (interior-node `baz
                               (leaf 11)
                               (interior-node `quux
                                              (leaf 117)
                                              (leaf 14)))))