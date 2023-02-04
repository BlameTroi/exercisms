;;; hamming.el --- Hamming (exercism)  -*- lexical-binding: t; -*-

;; Author: Troy Brumley <blametroi@gmail.com>

;;; Commentary:

;; This is pretty straight forward. The lisp cond form allows us to
;; code guard clauses for special cases before looping character by
;; character.

;;; Code:

(defun hamming-distance (dna1 dna2)
  (cond
   ((not (= (length dna1) (length dna2))) (error "%s" "Strand lengths do not match"))
   ((string= dna1 dna2) 0)
   (t (let ((i 0) (d 0))
        (while (< i (length dna1))
          (if (not (= (aref dna1 i) (aref dna2 i)))
              (setq d (1+ d)))
          (setq i (1+ i)))
        d))))

;; (hamming-distance "" "dna2")
;; (hamming-distance "dna1" "")
;; (hamming-distance "dna1" "dna10")
;; (hamming-distance "CTAG" "CTAG")
;; (hamming-distance "CTAC" "GCCG")

(provide 'hamming)
;;; hamming.el ends here
