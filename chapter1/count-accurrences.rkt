#lang racket

(define (count-occurrences s slist)
  (if (null? slist)
    0
    (if (eqv? (car slist) s)
      (+ 1 (count-occurrences s (cdr slist)))
      (if (symbol? (car slist))
       (count-occurrences s (cdr slist))
       (+ (count-occurrences s (car slist))
          (count-occurrences s (cdr slist)))))))
