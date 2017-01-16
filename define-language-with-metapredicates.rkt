#lang racket ; File define-language-with-metapredicate.rkt
(require redex)
(provide define-language-with-metapredicates)

#| define-language-with-metapredicates is like define-language of redex, but
allows each clause to be adorned with a predicate by means of keyword #:pred. |#

(define-syntax (define-language-with-metapredicates stx)
 (syntax-case stx ()
  ((define-language-with-metapredicates language clause ...)
   (letrec
    ((extract-language-clauses
      (λ (clauses)
       (map extract-language-clause (syntax->list clauses))))
     (extract-language-clause
      (λ (clause)
       (syntax-case clause ()
        ((id pattern ... #:pred pred) #'(id pattern ...))
        ((id pattern ...) #'(id pattern ...)))))
     (extract-predicates
      (λ (clauses)
       (filter (λ (x) x) (map extract-predicate (syntax->list clauses)))))
     (extract-predicate
      (λ (clause)
       (syntax-case clause ()
        ((id pattern ... #:pred pred) #'(pred id))
        ((id pattern ...) #f)))))
    (with-syntax
     (((clause ...) (extract-language-clauses #'(clause ...)))
      (((predicate id) ...) (extract-predicates #'(clause ...))))
  #'(begin
     (define-language language clause ...)
     (define-metafunction language predicate : any -> any
      ((predicate id) #t) ((predicate any) #f)) ...))))))