#lang racket
;Procedure (shortest-paths S T G) -> (P ...)

;For any type N:

;S	: N	starting node
;T	: N	destination node
;G	: N → (N ...)	procedure representing a graph
;eq	: N N → Boolean	equality relation
;P	: (N ...)	path from S to T

(define (shortest-paths S T G)
 (define H (make-hash))
 (define (put! P) (hash-set! H (car P) #f))
 (define (new? N) (not (hash-has-key? H N)))
 (define (filtered-G N) (filter new? (G N)))
 (define (extend P)
  (let ((Ns (remove-duplicates (filtered-G (car P)))))
   (map (lambda (N) (cons N P)) Ns)))
 (define (next-F F)
  (if (null? F) '( ) ; No paths found.
   (let ((Ps (filter (lambda (P) (equal? (car P) T)) F)))
    (if (pair? Ps) (map reverse Ps) ; The paths have been found.
     (begin (for-each put! F) ; Point T has not yet been found.
      (next-F (apply append (filter pair? (map extend F)))))))))
 (next-F (list (list S))))

;;; test

(define (make-point x y) (list x y))
(define x-coordinate car)
(define y-coordinate cadr)
(require (only-in redex test-equal test-results))

(define (test-shortest-paths from to)
 (define (G P)
  (let ((x (x-coordinate P)) (y (y-coordinate P)))
   (list
    (make-point x (add1 y))
    (make-point x (sub1 y))
    (make-point (add1 x) y)
    (make-point (sub1 x) y))))
 (test-equal (length (shortest-paths from to G))
  (let
   ((x (abs (- (x-coordinate from) (x-coordinate to))))
    (y (abs (- (y-coordinate from) (y-coordinate to)))))
   (binomial (+ x y) x))))

(define-syntax define/c
 (syntax-rules ()
  ((_ (name arg ...) . body)
   (define name
    (let ((cache (make-hash)))
     (lambda (arg ...)
      (let ((key (list arg ...)))
       (apply values
        (hash-ref cache key
         (lambda ()
          (let ((r (call-with-values (lambda () . body) list)))
           (hash-set! cache key r)
           r)))))))))))

(define/c (binomial n k) (/ (factorial n) (factorial k) (factorial (- n k))))
(define/c (factorial k) (if (zero? k) 1 (* k (factorial (sub1 k)))))

(let*
 ((from (make-point 0 0))
  (range (in-range -5 6))
  (points (for*/list ((x range) (y range)) (make-point x y))))
 (for ((to points)) (test-shortest-paths from to)))

(test-results)
