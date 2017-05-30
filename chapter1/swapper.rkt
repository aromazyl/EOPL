#lang racket

(define (swapper b a lst)
  (define (replace elem)
    (cond
      [(symbol? elem)
       (cond
         [(eq? a elem) b]
         [(eq? b elem) a]
         [else elem])]
      [else (swapper b a elem)]))
  (map replace lst))
