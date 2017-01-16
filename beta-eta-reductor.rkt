#lang racket ; file beta-eta-reductor.rkt
(require "curried-lc-grammar.rkt" "beta-reductor.rkt" "free-vars.rkt" redex)
(provide βη-reductor βηnf?)

(define βη-reductor
 (extend-reduction-relation β-reductor curried-lc-grammar
  (--> (in-hole ‹subterm› (λ (‹var›_0) (‹term› ‹var›_0)))
       (in-hole ‹subterm› ‹term›)
       (side-condition (not (term (Var-free-in? ‹var›_0 ‹term›))))
       "η")))

(define-metafunction curried-lc-grammar βηnf? : ‹term› -> ‹bool›
 ((βηnf? ‹term›) ,(null? (apply-reduction-relation βη-reductor (term ‹term›)))))
