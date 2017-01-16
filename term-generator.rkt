#lang racket ; File term-generator
(require redex)
(printf "~a~n" "term-generator")

(define-language x-lang
 (‹xterm› X (‹xterm› ‹xterm›))
 (‹hole› (‹hole› ‹xterm›) (‹xterm› ‹hole›) hole))

(define extend
 (reduction-relation x-lang
  (--> (in-hole ‹hole› X) (in-hole ‹hole› (X X)))))

(define (list-xterms n) ; Lists all xterms with n occurrences of X.
 (case n
  ((0) '( ))  ((1) '(X)) ((2) '((X X)))
  (else (remove-duplicates (apply append (map apply-extend (list-xterms (sub1 n))))))))

(define-metafunction x-lang Term<? : ‹xterm› ‹xterm› -> any
 ((Term<? X X) #f)
 ((Term<? X ‹xterm›) #t)
 ((Term<? ‹xterm› X) #f)
 ((Term<? (‹xterm›_0 ‹xterm›_1) (‹xterm›_0 ‹xterm›_2)) (Term<? ‹xterm›_1 ‹xterm›_2))
 ((Term<? (‹xterm›_0 ‹xterm›_1) (‹xterm›_2 ‹xterm›_3)) (Term<? ‹xterm›_0 ‹xterm›_2)))

(define (Catalan n) (quotient (! (* 2 n)) (* (! n) (! (add1 n)))))  ; (2n)!/(n!(n+1)!)
; These are Catalan numbers after the mathematician Eugène Charles Catalan.
; They solve many counting problems. Here they are used to count the number of fully
; curried xterms with n+1 occurrences of X, n>0.

(define (sort-xterms x) (sort x xterm<?))
(define (xterm<? x y) (term (Term<? ,x ,y)))
(define (apply-extend x) (apply-reduction-relation extend x))
(define (! n) (if (zero? n) 1 (* n (! (sub1 n)))))

(define (test n #:print (print-x-terms #t))
 (for ((n (in-range 1 n)))
  (let* ((r (list-xterms n)) (c (length r)) (C (Catalan (sub1 n))))
   (printf "nr of X: ~s, nr of xterms: ~s~n" n c)
   (when print-x-terms (for-each (λ (r) (printf "~s~n" r)) (sort-xterms r)) (newline))
   (test-equal c C)))
 (test-results))

(test 10 #:print #t)
