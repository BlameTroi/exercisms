(import (rnrs))             ;; expected by testing framework
(use-modules (srfi srfi-1)) ;; last item in list

;; Compute Pascal's triangle up to a given number of rows.
;;
;; In Pascal's Triangle each number is computed by adding the
;; numbers to the right and left of the current position in
;; the previous row.
;;
;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;; # ... etc
;;
;; or with a lean:
;;
;; row         n   calculation
;; 1           1   end elements 1 by definition
;; 1 1         2
;; 1 2 1       3   2 = 1 + 1
;; 1 3 3 1     4   3 = 2 + 1  3 = 1 + 2
;; 1 4 6 4 1   5   4 = 3 + 1  6 = 3 + 3  4 = 1 + 3
;;
(define (pascals-triangle n)
  (cond
   ;; first 4 results cooked ahead
   ((= 0 n) '( ))
   ((= 1 n) '( (1) ))
   ((= 2 n) '( (1) (1 1) ))
   ((= 3 n) '( (1) (1 1) (1 2 1) ))
   ;; compute all but first and last entries for this
   ;; row, which are 1 by definition.
   (else
    (let*
        ((curr (make-vector n 1))
         (prior (pascals-triangle (1- n)))
         (pvec  (list->vector (last prior)))
         (i 1))
      (while (< i (1- n))
        (vector-set! curr i (+ (vector-ref pvec (1- i)) (vector-ref pvec i)))
        (set! i (1+ i)))
      (append prior (list (vector->list curr)))))))
