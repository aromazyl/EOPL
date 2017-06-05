#lang eopl

(define val? (lambda (s) #t))
(define-datatype stack stack?
                 (empty-stack)
                 (push
                   (val val?)
                   (saved-stack stack?)))

(define (pop stk)
  (cases stack stk
         (empty-stack ()
                      `())
         (push (val saved-stack)
               saved-stack)))

(define (top stk)
  (cases stack stk
         (empty-stack ()
                      `())
         (push (val saved-stack)
               val)))

(define (empty-stack? stk)
  (cases stack stk
         (empty-stack ()
                      #t)
         (push (val saved-stack)
               #f)))
