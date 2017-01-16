#lang racket

(define x 'x) ; necessary, because Racket does not allow free variables.

((λ (x) (λ (x) x)) x)