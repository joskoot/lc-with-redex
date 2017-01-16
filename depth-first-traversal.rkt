#lang racket

;;; Why depth-first traversal does not work.

(define (traverse-depth-first S G)
 (define (traverse-depth-first N) (for-each traverse-depth-first (G N)))
 (traverse-depth-first S))

(define (make-point x y) (list x y))
(define x-coordinate car)
(define y-coordinate cadr)

(let/cc cc
 (let ((n 11))
  (define (G point)
   (if (zero? n) (cc) ; exit after having found 10 points
    (let ((x (x-coordinate point)) (y (y-coordinate point)))
     (set! n (sub1 n))
     (printf "Point found: ~s. " (list x y))
     (when (> x 0) (printf "Points with x<~s are lost. " x))
     (when (> y 0) (printf "Points with y<~s are lost. " y))
     (newline)
     (let ((down (make-point x (add1 y))) (right (make-point (add1 x) y)))
      (if (zero? (random 2)) ; return the two nodes in arbitrary order.
       (list down right)
       (list right down))))))
  (random-seed 0)
  (traverse-depth-first (make-point 0 0) G)))

#|
Displayed:
Point found: (0 0). 
Point found: (1 0). Points with x<1 are lost. 
Point found: (2 0). Points with x<2 are lost. 
Point found: (2 1). Points with x<2 are lost. Points with y<1 are lost. 
Point found: (2 2). Points with x<2 are lost. Points with y<2 are lost. 
Point found: (3 2). Points with x<3 are lost. Points with y<2 are lost. 
Point found: (3 3). Points with x<3 are lost. Points with y<3 are lost. 
Point found: (4 3). Points with x<4 are lost. Points with y<3 are lost. 
Point found: (4 4). Points with x<4 are lost. Points with y<4 are lost. 
Point found: (4 5). Points with x<4 are lost. Points with y<5 are lost. 
Point found: (5 5). Points with x<5 are lost. Points with y<5 are lost.
|#
