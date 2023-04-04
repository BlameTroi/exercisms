(import (rnrs))

(define (attacking? white black)
  "On an 8x8 chess board with rows and columns ranging from
0 through 7 can two queens attack each other.

No input validation required."
  ;; helpers
  (define (row q) (car q))
  (define (col q) (cadr q))
  (define (coord=? q c)
    (equal? q c))
  (define (on-diagonal? w b)
    (let ((done #f))
      ;; check along diagonals of queen w to see if queen b
      ;; is on the line. the do loops set done to #t if
      ;; there's a hit, and subsequent dos will not reset
      ;; it and will fall through to exit.
      ;; upper left
      (do ((r (row w) (1- r)) (c (col w) (1- c)))
          ((or done (< r 0) (< c 0)) done)
        (set! done (coord=? b (list r c))))
      ;; lower left
      (do ((r (row w) (1+ r)) (c (col w) (1- c)))
          ((or done (> r 7) (< c 0)) done)
        (set! done (coord=? b (list r c))))
      ;; upper right
      (do ((r (row w) (1- r)) (c (col w) (1+ c)))
          ((or done (< r 0) (> c 7)) done)
        (set! done (coord=? b (list r c))))
      ;; lower right
      (do ((r (row w) (1+ r)) (c (col w) (1+ c)))
          ((or done (> r 7) (> c 7)) done)
        (set! done (coord=? b (list r c))))))
  ;; check
  (cond
   ((= (row white) (row black)) #t)
   ((= (col white) (col black)) #t)
   (else (on-diagonal? white black))))
