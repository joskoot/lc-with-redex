#lang racket ; File test-pairs.rkt"
(require redex "pairs.rkt" "lazy-evaluator.rkt")
(printf "~a~n" "test-pairs")
(test-equal (ev (Car (Cons yes no))) 'yes)
(test-equal (ev (Cdr (Cons yes no))) 'no)
(test-results)
