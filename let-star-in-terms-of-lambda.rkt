#lang racket ; File let-star-in-terms-of-lambda.rkt
(define-syntax let*
 (syntax-rules ()
  ((let* () . body) ((λ () . body)))
  ((let* ((var expr) binding ...) . body)
   ((λ (var) (let* (binding ...) . body)) expr))))

(let* ((a 1) (b 2) (c (+ a b)) (a (+ b c)) (c (+ a b c))) (list a b c)) ; --> (5 2 10)
 
