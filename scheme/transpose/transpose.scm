(import (rnrs))
(use-modules (srfi srfi-1))

;;;
;;; This exercism is *broken*. Somewhere along the
;;; line maintenance squared up the test input by
;;; padding it with blanks. If you compare with
;;; say transpose on the go track, you'll see that
;;; the input is supposed to be ragged. Dealing
;;; with the different row lengths is a significant
;;; part of the exercise.
;;;
;;; Also, the input should be expressed as readable
;;; characters to make the instructions from the
;;; readme relatable. Instead of
;;
;; '((72 69 65 82 84)
;;   (69 77 66 69 82)
;;   (65 66 85 83 69)
;;   (82 69 83 73 78)
;;   (84 82 69 78 68))
;;
;; '((#\H #\E #\A #\R #\T)
;;   (#\E #\M #\B #\E #\R)
;;   (#\A #\B #\U #\S #\E)
;;   (#\R #\E #\S #\I #\N)
;;   (#\T #\R #\E #\N #\D))
;;
;; Or more meaningfully
;;
;; '((84 104 101 32 102 105 114 115 116 32 108 105 110 101 46)
;;  (84 104 101 32 115 101 99 111 110 100 32 108 105 110 101 46))))
;;
;; '((#\T #\h #\e #\space #\f #\i #\r #\s #\t #\space #\l #\i #\n #\e #\.)
;;   (#\T #\h #\e #\space #\s #\e #\c #\o #\n #\d #\space #\l #\i #\n #\e #\.))
;;

;;;
;;; Since the input cases are fixed (or broken, depends on your point
;;; of view) the solution is trivial.
;;;

(define (transpose matrix)
  "Transpose MATRIX represented as a list of lists."
  (cond ((null? matrix) '())
        (else (apply zip matrix))))


;;;
;;; notes from the instructions in the readme:
;;;
;; Given an input text, output it transposed.
;;
;; Roughly explained, the transpose of the left
;; matrix gives the one on the right:
;;
;; ABC     AD
;; DEF     BE
;;         CF
;;
;; Rows become columns and columns become rows.
;;
;; If the input has rows of different lengths:
;; * Pad to the left with spaces.
;; * Don't pad to the right.
;;
;; Therefore we see:
;;
;; ABC     AD   and     AB      AD
;; DE      BE           DEF     BE
;;         C                     F
;;
;; In general, all characters from the input
;; should also be present in the transposed
;; output. That means that if a column in the
;; input text contains only spaces on its bottom-
;; most row(s), the corresponding output row
;; should contain the spaces in its right-most
;; column(s).
