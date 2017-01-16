#lang racket

(define make-factorial-6
 (λ (make-factorial)
  ((λ (factorial)
    (λ (n) (if (zero? n) 1 (* n (factorial (sub1 n))))))
   (make-factorial make-factorial))))

(define factorial-6 (make-factorial-6 make-factorial-6))

(factorial-6 4)
