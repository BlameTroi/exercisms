(import (rnrs))

(define (square n)
  "How many grains of wheat are on square N of
a chessboard? N ranges from 1 to 64.

A recursive solution powers of two."
  (cond ((= n 1) 1)
        ((or (> n 64) (< n 1)) (error 'square "illegal square" n))
        (else (* 2 (square (1- n))))))

(define (total)
  "How many grains of wheat are on a full
chessboard, where each square holds twice the
grains as the square preceeding it."
  (let ((n 1) (s 0))
    (while (< n 65)
      (set! s (+ s (square n)))
      (set! n (1+ n)))
    s))

;; Optimizations probably don't help this too
;; much.
;;
;; Square uses tail recursion. Finding a
;; library to do bit shifts (the truely fast
;; way to do powers of 2) doesn't seem worth
;; it.
;;
;; Memoization of prior calculations might speed
;; things up, but this completes so quickly on
;; on my years old laptop why bother?
