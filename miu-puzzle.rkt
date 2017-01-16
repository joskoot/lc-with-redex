#lang racket (require redex) ; File miu-puzzle.rkt
(provide MIU-rules MIU-language axiom traces)

(define-language MIU-language ; Grammar
 (‹sentence› (‹symbol› ...))  ; ‹sentence› is a sentence of the language.
 (‹symbol› M I U))            ; ‹symbol› is one of the symbols M, I or U.

(define axiom (term (M I)))

(define MIU-rules ; Semantics
 (reduction-relation MIU-language
  (-->
   (‹symbol› ... I)
   (‹symbol› ... I U)
   "rule 1")
  (-->
   (M ‹symbol› ...)
   (M ‹symbol› ... ‹symbol› ...)
   "rule 2")
  (-->
   (‹symbol›_0 ... I I I ‹symbol›_1 ...)
   (‹symbol›_0 ... U ‹symbol›_1 ...)
   "rule 3")))
