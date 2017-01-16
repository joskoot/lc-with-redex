#lang racket
(require redex)

;; A simple reductor for arithmetic expressions consisting of
;; parentheses, plus sighs, minus signs, multiplication signs and numbers.

(define-language arithmetic
 (‹expr› number
         (‹expr›)
         (‹sign› ‹expr›)
         (‹expr› ‹operator› ‹expr›))
 (‹subexpr›
         (‹sign› ‹subexpr›)
         (‹subexpr› ‹operator› ‹expr›)
         (‹expr› ‹operator› ‹subexpr›)
         hole)
 (‹sign› + -)
 (‹operator› ‹sign› *))

(define reduce
 (reduction-relation arithmetic
  (--> (in-hole ‹subexpr› (‹expr›))
       ; remove superfluous parentheses.
       (in-hole ‹subexpr› ‹expr›))
  (--> (in-hole ‹subexpr› (+ number))
       ; remove superfluous unary plus sign.
       number)
  (--> (in-hole ‹subexpr› (- number))
       ; apply unary minus sign.
       ,(- (term number)))
  (--> (in-hole ‹subexpr› (number_0 + number_1))
       ; binary addition
       (in-hole ‹subexpr› ,(+ (term number_0) (term number_1))))
  (--> (in-hole ‹subexpr› (number_0 - number_1))
       ; binary subtraction
       (in-hole ‹subexpr› ,(- (term number_0) (term number_1))))
  (--> (in-hole ‹subexpr› (number_0 * number_1))
       ; binary multiplication
       (in-hole ‹subexpr› ,(* (term number_0) (term number_1))))
  (--> (in-hole ‹subexpr› ((‹expr›_0 ‹sign› ‹expr›_1) * ‹expr›_2))
       ; distribution rule
       (in-hole ‹subexpr› ((‹expr›_0 * ‹expr›_2) ‹sign› (‹expr›_1 * ‹expr›_2))))
  (--> (in-hole ‹subexpr› (‹expr›_2 * (‹expr›_0 ‹sign› ‹expr›_1)))
       ; distribution rule
       (in-hole ‹subexpr› ((‹expr›_0 * ‹expr›_2) ‹sign› (‹expr›_1 * ‹expr›_2))))
))

(parameterize ((reduction-steps-cutoff 200))
 (traces reduce (term ((3 + 4) * (5 + 6)))
  #:pred
  ; Color the result pink.
  ; Check that everything else than a number has a reduct.
  (λ (T)
   (cond
    ((number? T) #f)
    ((null? (apply-reduction-relation reduce T))
     (error 'reduce "no reducts found for: ~s" T))
    (else #t)))))

; Check that the graph contains the final result 77 and that this is the only box
; that does not have an outbound branch. You may have to scroll a bit to find 77.

(stepper reduce '((3 + 4) * (5 + 6)))
                                     
