#lang racket

(define-datatype bintree bintree?
                 (leaf-node
                   (num integer?))
                 (interior-node
                   (key symbol?)
                   (left bintree?)
                   (right bintree?)))
