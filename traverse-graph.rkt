#lang racket
#|
Width first traversal of a graph starting from a given staring node.

Procedure (traverse-graph G S) -> (T ...)
For arbitrary node type T:
G : T -> (T ...)
S : T

Procedure G must given a node T return the list of all nodes T has outbound branches to. The returned list may contain multiple occurences of the same node. The second and follwing occurrence of the same node will be ignored. Nodes are compared to each other with procedure equal?.

Procedure traverse-graph returns a list of all nodes that can be reached starting from the starting node S. The returned list contains no duplicates. The list is sorted in decreasing distance from the starting node, assuming each branch has the same positove length. 

R : (T ...) list of nodes found so far.
H : (T ...) list of nodes whose outbound branches have not yet been followed.
N : (T ...) list of new nodes found while following branches of the nodes of H.
B : (T ...) list of nodes with inbound branches from element of H.
K : T       element of B
|#

(define (traverse-graph S G)
 (define D (make-hash))
 (hash-set! D S
 (let ((R (list S)))
  (let loop ((R R) (H R))
   (if (null? H) R
    (let H-loop ((R R) (H H) (N '()))
     (if (null? H) (loop R N)
      (let B-loop ((R R) (B (G (car H))) (N N))
       (if (null? B) (H-loop R (cdr H) N)
        (let ((K (car B)) (B (cdr B)))
         (if (member K R) (B-loop R B N)
          (B-loop (cons K R) B (cons K N))))))))))))

(provide traverse-graph)

(require redex)

(define (Collatz-next n) (if (even? n) (/ n 2) (add1 (* 3 n))))
(define (Collatz-length n) (if (= n 1) 1 (add1 (Collatz-length (Collatz-next n)))))

(define (test-Collatz n)
 (test-equal
  (let ((r 0))
   (traverse-graph n
    (λ (node) ; reversed Collatz sequence (linear graph)
     (set! r (add1 r))
     (if (= node 1) '() (list (Collatz-next node)))))
   r)
  (Collatz-length n)))

(time (for ((i (in-range 1 100))) (test-Collatz i)))

(define (test-finite-rectangular-grid nx ny)
 (define R '())
 (define (G T) ; Rectangular grid with branches down and to the right
  (when (member T R) (error 'test-finite-rectangular-grid "duplicate node"))
  (set! R (cons T R))
  (let ((x (car T)) (y (cadr T)))
   ((λ (new) (append new new new))
    (filter (λ (p) p)
     (list
      (and (< x nx) (list (add1 x) y))
      (and (< y ny) (list x (add1 y))))))))
  (test-equal
   (begin (traverse-graph '(0 0) G) (sort-number-pairs R))
   (for*/list ((x (in-range 0 (add1 nx))) (y (in-range 0 (add1 ny)))) (list x y))))

(define (sort-number-pairs x)
 (sort x
  (λ (x y)
   (or (< (car x) (car y))
    (and (= (car x) (car y)) (< (cadr x) (cadr y)))))))

(time (for* ((x (in-range 10 30)) (y (in-range 20 40))) (test-finite-rectangular-grid x y)))

(define (test-fully-connected n)
 (define nodes (build-list n (λ (k) k)))
 (define H (make-hash))
 (define (G T)
  (when (hash-has-key? H T) (error 'test-fully-connected "duplicate node ~s" T))
  (hash-set! H T #t)
  nodes)
 (traverse-graph 0 G)
 (test-equal (sort (hash-map H (λ (x y) x)) <) nodes))

(time (for ((n (in-range 1 100))) (test-fully-connected n)))

(define (test-infinite-graph limit)
 (let ((n limit))
  (test-equal
   (let/ec ec
    (define R '())
    (define (G T)
     (if (zero? n) (ec (length R))
      (begin
       (set! n (sub1 n))
       (set! R (cons T R))
       (for/list ((T (in-range (add1 T) (+ T 10)))) T))))
    (traverse-graph 0 G))
   limit)))

(time (for ((limit (in-range 0 100))) (test-infinite-graph limit)))

(test-results)
(collect-garbage)
