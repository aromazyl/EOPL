#lang racket

(define empty-stack? null?)
(define val? (lambda (s) #t))
(define-datatype stack stack?
                 (empty-stack)
                 (push
                   (val val?)
                   (saved-stack stack?)))

(define (pop stk)
  (cases stack mstk
         (empty-stack ()
                      (eopl::error `pop "Empty Stack"))
         (push (val saved-stack)
               saved-stack)))

(define (top stk)
  (cases stack mstk
         (empty-stack ()
                      (eopl::error `top "Empty Stack"))
         (push (val saved-stack)
               val)))

(define (empty-stack? stk)
  (cases stack mstk
         (empty-stack ()
                      #t)
         (push (val saved-stack)
               #f)))
