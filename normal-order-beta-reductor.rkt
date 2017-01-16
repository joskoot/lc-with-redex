#lang racket ; File normal-order-beta-reductor-version-1.rkt

(require redex "tracer.rkt")
(require redex "curried-lc-grammar.rkt")
(require (only-in "beta-reductor.rkt" Subst))
(provide normal-order-β-reductor βNf? β-redex?)

(define normal-order-β-reductor
 (reduction-relation curried-lc-grammar
  (--> ‹term› (Contract-once ‹term›)
   (side-condition (not (term (βNf? ‹term›)))))))

(define-metafunction curried-lc-grammar βNf? : ‹term› -> ‹bool›
 ((βNf? ‹var›) #t)
 ((βNf? (λ (‹var›) ‹term›)) (βNf? ‹term›))
 ((βNf? (‹term›_0 ‹term›_1))
 ,(and
   (not (term (Abstr? ‹term›_0)))
   (term (βNf? ‹term›_0))
   (term (βNf? ‹term›_1)))))

(define-metafunction curried-lc-grammar β-redex? : ‹term› -> ‹bool›
 ((β-redex? ((λ (‹var›) ‹term›_0) ‹term›_1)) #t)
 ((β-redex? any) #f))
 
(define-metafunction curried-lc-grammar Contract-once : ‹term› -> ‹term›
 ((Contract-once (side-condition ‹term› (term (β-redex? ‹term›))))
  (Contract-β-redex ‹term›))
 ((Contract-once (λ (‹var›) ‹term›)) (λ (‹var›) (Contract-once ‹term›)))
 ((Contract-once
   ((side-condition ‹term›_0 (not (term (βNf? ‹term›_0)))) ‹term›_1))
  ((Contract-once ‹term›_0) ‹term›_1))
 ((Contract-once (‹term›_0 ‹term›_1)) (‹term›_0 (Contract-once ‹term›_1))))

(define-metafunction curried-lc-grammar Contract-β-redex : ‹term› -> ‹term›
 ((Contract-β-redex ((λ (‹var›_0) ‹term›_0) ‹term›_1))
  (Subst (‹var›_0 ‹term›_1) ‹term›_0)))

(printf "~a~n" "test-normal-order-β-reductor")

(tracer normal-order-β-reductor
 (((((λ (x y) x) y) z) ((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z))
  (y (a (a (a (a z))))))
 (((((λ (z) (z z)) (λ (x y) (x (x y)))) a) z) (a (a (a (a z)))))
 ((λ (x) (y x)) (λ (x) (y x)))
 (((λ (x) x) y) y)
 (((λ (x y z) (x y z)) a b c) (a b c))
 (((λ (z) ((a b) z)) c) (a b c))
 (((λ (x) (x x)) (λ (x) (x x))) #f))

(redex-check curried-lc-grammar ‹term›
 (let*
  ((nf? (term (βNf? ‹term›)))
   (redices
    (apply-reduction-relation
     normal-order-β-reductor
     (term ‹term›))))
  (cond
   ((and (pair? redices) (pair? (cdr redices)))
    (error 'test "more than one left most redex for term ~s"
     (term ‹term›)))
   ((and nf? (pair? redices))
    (error 'test "(βNf? ~s)->#t but reductor found redex ~s."
     (term ‹term›) (car redices)))
   ((and (not nf?) (null? redices))
    (error 'test "(βNf? ~s)->#f but reductor found no redex ~s."
     (term ‹term›)))))
 #:attempts 100000 #:retries 100)

