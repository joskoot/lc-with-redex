#lang racket ; file pairs.rkt"
(require "booleans.rkt" "lazy-evaluator.rkt")
(def Cons (λ (x y z) (If z x y)))
(def Car  (λ (x) (x True)))
(def Cdr  (λ (x) (x False)))

