#lang racket ; file test-beta-reductor.rkt"
(require redex "beta-reductor.rkt" "tracer.rkt")
(printf "~a~n" "test-beta-reductor")

(parameterize ((reduction-steps-cutoff 100))
 (tracer β-reductor
  ((x y z) (x y z))
  ((λ (x) x) (λ (y) y))
  ((λ (x) (F x)) (λ (x) (F x)))
  ((λ (x) ((λ (y) y) x)) (λ (x) x))
  (((λ (x) (y x)) x) (y x))
  (((λ (x y) x) y) (λ (p) y))
  (((λ (x) (λ (y) (x y))) z) (λ (y) (z y)))
  (((λ (x y z) (x y z)) a b c) (a b c))
  (((λ (x) (x x)) (λ (x) (x x))) #f)
  ((λ (p) ((λ (x) (x x)) (λ (x) (x x)))) #f)
  (((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z) (a (a (a (a z)))))))

(parameterize ((reduction-steps-cutoff 10))
 (tracer β-reductor (((λ (x) (x x x)) (λ (x) (x x x))) #f)))
