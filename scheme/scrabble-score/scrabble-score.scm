(import (rnrs))

(define (score word)
  "Compute the Scrabble score for WORD."
  (cond ((null? word) 0)
        ((string? word) (score (string->list word)))
        (else (+ (letter-value (car word)) (score (cdr word))))))

(define (letter-value c)
  "Returns the score for an individual letter in Scrabble."
  (cond
   ((char-lower-case? c) (letter-value (char-upcase c)))
   ((not (char-upper-case? c)) 0)
   (else ;; a table lookup would be better, but case/switch works
    (cond
     ((or (char=? c #\Q) (char=? c #\Z)) 10)
     ((or (char=? c #\J) (char=? c #\X)) 8)
     ((char=? c #\K)                     5)
     ((or (char=? c #\F) (char=? c #\H)
          (char=? c #\V) (char=? c #\W)
          (char=? c #\Y))                4)
     ((or (char=? c #\B) (char=? c #\C)
          (char=? c #\M) (char=? c #\P)) 3)
     ((or (char=? c #\D) (char=? c #\G)) 2)
     (else 1)))))

;; scoring table
;;
;; Letter                           Value
;; A, E, I, O, U, L, N, R, S, T       1
;; D, G                               2
;; B, C, M, P                         3
;; F, H, V, W, Y                      4
;; K                                  5
;; J, X                               8
;; Q, Z                               10
