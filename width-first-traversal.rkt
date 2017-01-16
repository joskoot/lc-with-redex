#lang racket

;Procedure: (traverse-graph S G) → any
;For arbitrary node type N:
;S : N                   starting node.
;G : N -> (N ...)        graph.

(define (traverse-graph S G)
 (define H (make-hash)) ; Memorizes all nodes that have already been found..
 (define (put! N) (hash-set! H N #f)) ; Mark node N as being found.
 (define (new? N) (not (hash-has-key? H N))) ; Do we have a new node?
 (define (filtered-G F) (filter new? (G F))) ; Returns new nodes only.
 (define (next-F F) ; F = nodes whose outbound branches must yet be followed.
  (unless (null? F)
   (for-each put! F)
   (next-F (remove-duplicates (apply append (map filtered-G F))))))
 (next-F (list S)))

(define (make-point x y) (list x y))
(define x-coordinate car)
(define y-coordinate cadr)

(let/cc cc
 (let ((n 20))
  (define (G point)
   (if (zero? n) (cc) ; exit after having found 20 points
    (let ((x (x-coordinate point)) (y (y-coordinate point)))
     (set! n (sub1 n))
     (printf "Point found: ~s (diagonal ~s)~n" (list x y) (+ x y))
     (let ((down (make-point x (add1 y))) (right (make-point (add1 x) y)))
      (if (zero? (random 2))
       (list down right)
       (list right down))))))
  (random-seed 0)
  (traverse-graph (make-point 0 0) G)))

#|
Displayed:
Point found: (0 0) (diagonal 0)
Point found: (1 0) (diagonal 1)
Point found: (0 1) (diagonal 1)
Point found: (2 0) (diagonal 2)
Point found: (1 1) (diagonal 2)
Point found: (0 2) (diagonal 2)
Point found: (2 1) (diagonal 3)
Point found: (3 0) (diagonal 3)
Point found: (1 2) (diagonal 3)
Point found: (0 3) (diagonal 3)
Point found: (3 1) (diagonal 4)
Point found: (2 2) (diagonal 4)
Point found: (4 0) (diagonal 4)
Point found: (1 3) (diagonal 4)
Point found: (0 4) (diagonal 4)
|#

#| In the following example the graph is infinite. The graph contains exactly one node for each natural number and is almost a tree. Starting from 0, there is a path to every natural number. Each natural number n has one inbound branch from node (quotient n 10) and 10 outbound branches to the nodes 10n+i for i running through the natural numbers from 0 up to and including 9. The graph is traversed starting from 0. It is protected by means of an escape continuation such as to halt when number stop is found. Procedure traverse-graph guarantees that it finds every arbitrary natural number given for the stop argument. |#

(define (test stop)
 (test-equal stop
  (let/cc cc
   (define H (make-hash))
   (define (G n)
    (cond
     ; Exit when stop is found.
     ((= n stop) (cc stop))
     ; Check that n has not yet been processed. 
     ((hash-has-key? H n) (error 'test "duplicate node" n))
     ; Declare n processed and return natural numbers 10n+i for 0≤i<10.
     (else (hash-set! H n #f)
      (let ((inc (let ((k (* 10 n))) (lambda (i) (+ k i)))))
       (build-list 10 inc)))))
   (traverse-graph 0 G))))

(require (only-in redex test-equal test-results))
(for ((stop (in-range 0 100))) (test stop))
(test-results)

