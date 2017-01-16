#lang racket ; File test-booleans.rkt"
(require redex "booleans.rkt" "lazy-evaluator.rkt")
(printf "~a~n" "test-booleans")
(test-equal (ev (    True              yes no)) 'yes)
(test-equal (ev (    False             yes no)) 'no )
(test-equal (ev (Not False             yes no)) 'yes)
(test-equal (ev (Not True              yes no)) 'no )
(test-equal (ev (And True  True        yes no)) 'yes)
(test-equal (ev (And True  False       yes no)) 'no )
(test-equal (ev (And False True        yes no)) 'no )
(test-equal (ev (And False False       yes no)) 'no )
(test-equal (ev (Or  True  True        yes no)) 'yes)
(test-equal (ev (Or  True  False       yes no)) 'yes)
(test-equal (ev (Or  False True        yes no)) 'yes)
(test-equal (ev (Or  False False       yes no)) 'no )
(test-equal (ev (If  True              yes no)) 'yes)
(test-equal (ev (If  False             yes no)) 'no )
(test-equal (ev (If  (Not False)       yes no)) 'yes)
(test-equal (ev (If  (Not True)        yes no)) 'no )
(test-equal (ev (If  (And True  True)  yes no)) 'yes)
(test-equal (ev (If  (And True  False) yes no)) 'no )
(test-equal (ev (If  (And False True ) yes no)) 'no )
(test-equal (ev (If  (And False False) yes no)) 'no )
(test-equal (ev (If  (Or  True  True ) yes no)) 'yes)
(test-equal (ev (If  (Or  True  False) yes no)) 'yes)
(test-equal (ev (If  (Or  False True ) yes no)) 'yes)
(test-equal (ev (If  (Or  False False) yes no)) 'no )

(test-results) ; Display: All 24 tests passed.