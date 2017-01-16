#lang racket ; File test-curry.rkt"
(require redex "curry.rkt" "uncurried-lc-grammar.rkt" "curried-lc-grammar.rkt")
(printf "~a~n" "test-curry")

(test-equal
 (term (Curry ((λ (x x x) (x x x)) (λ (x x x) (x x x)) (λ (x x x) (x x x)))))
 (term
  (((λ (x) (λ (x) (λ (x) ((x x) x)))) (λ (x) (λ (x) (λ (x) ((x x) x)))))
   (λ (x) (λ (x) (λ (x) ((x x) x)))))))

(test-equal (term (Curry ((((x)))))) (term x))
(test-equal (term (Curry ((((λ () (((((x) (y))))))))))) (term (x y)))

(redex-check uncurried-lc-grammar ‹term›
 (term (Term? (Curry ‹term›))) #:attempts 2000 #:retries 100)

(test-results)
; Displays:
; redex-check: ...\test-curry.rkt":13 no counterexamples in 2000 attempts
; All 3 tests passed.
