#lang racket	; File free-vars.rkt
(require redex "curried-lc-grammar.rkt")
(provide Free-vars Var-free-in?)

(define-metafunction curried-lc-grammar Free-vars : ‹term› -> ‹varlist›
 ((Free-vars ‹var›) (‹var›))
 ((Free-vars (‹term›_0 ‹term›_1))
  (Merge (Free-vars ‹term›_0) (Free-vars ‹term›_1)))
 ((Free-vars (λ (‹var›) ‹term›))
  (Remove-var ‹var› (Free-vars ‹term›))))

(define-metafunction curried-lc-grammar
 Merge : ‹varlist› ‹varlist› -> ‹varlist›
 ((Merge (‹var›_0 ...) (‹var›_1 ...))
 ,(sort (remove-duplicates (term (‹var›_0 ... ‹var›_1 ...))) symbol<?)))

(define (symbol<? x y) (string<? (symbol->string x) (symbol->string y)))

(define-metafunction curried-lc-grammar
 Remove-var : ‹var› (‹var› ...) -> (‹var› ...)
 ((Remove-var ‹var›_0 (‹var›_1 ...))
 ,(remove (term ‹var›_0) (term (‹var›_1 ...)))))

(define-metafunction curried-lc-grammar Var-free-in? : ‹var› ‹term› -> ‹bool›
 ((Var-free-in? ‹var›_0 ‹var›_0) #t)
 ((Var-free-in? ‹var›_0 ‹var›_1) #f)
 ((Var-free-in? ‹var›_0 (λ (‹var›_0) ‹term›)) #f)
 ((Var-free-in? ‹var›_0 (λ (‹var›_1) ‹term›)) (Var-free-in? ‹var›_0 ‹term›))
 ((Var-free-in? ‹var› (‹term›_0 ‹term›_1))
 ,(or
   (term (Var-free-in? ‹var› ‹term›_0))
   (term (Var-free-in? ‹var› ‹term›_1)))))
