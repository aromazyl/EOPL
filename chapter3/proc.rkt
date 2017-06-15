#lang eopl
(require "./let-lang.rkt")

(define procedure
  (lambda (var body env)
    (lambda (val)
      (value-of body (extend-env var val env)))))

(define proc?
  (lambda (val)
    (procedure? val)))

(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)))


