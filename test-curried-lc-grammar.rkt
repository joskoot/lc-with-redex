#lang racket ; File test-curried-lc-grammar.rkt"
(require
 redex/reduction-semantics
 "curried-lc-grammar.rkt" "tools.rkt")
(printf "~a~n" "test-curried-lc-grammar")

(redex-check curried-lc-grammar ‹term›
 (=
  (apply +
   (map (λ (x) (if x 1 0))
    (list
     (term (Var?   ‹term›))
     (term (Abstr? ‹term›))
     (term (Appl?  ‹term›)))))
  1)
 #:attempts 10000 #:retries 100)

(test-false (term (Term?  123)))
(test-false (term (Var?   123)))
(test-false (term (Abstr? 123)))
(test-false (term (Appl?  123)))
(test-true  (term (Term?  x)))
(test-true  (term (Var?   x)))
(test-false (term (Abstr? x)))
(test-false (term (Appl?  x)))
(test-true  (term (Term?  (x y))))
(test-false (term (Var?   (x y))))
(test-false (term (Abstr? (x y))))
(test-true  (term (Appl?  (x y))))
(test-true  (term (Term?  (λ (x) y))))
(test-false (term (Var?   (λ (x) y))))
(test-true  (term (Abstr? (λ (x) y))))
(test-false (term (Appl?  (λ (x) y))))
(test-false (term (Term?  λ)))
(test-false (term (Term?  (λ (x y) z))))
(test-false (term (Term?  (λ (x)))))
(test-false (term (Term?  (x y z))))
(test-results)
