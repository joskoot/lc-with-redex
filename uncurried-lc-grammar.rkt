#lang racket ; File uncurried-lc-grammar.rkt"
(require redex "curried-lc-grammar.rkt")
(provide uncurried-lc-grammar)

(define-extended-language uncurried-lc-grammar curried-lc-grammar
 (‹term› ‹var› (λ (‹var› ...) ‹term›) (‹term› ‹term› ...)))

