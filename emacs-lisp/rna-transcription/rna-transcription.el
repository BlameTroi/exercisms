;;; rna-transcription.el -- RNA Transcription (exercism)  -*- lexical-binding: t; -*-

;; Author: Troy Brumley.

;;; Commentary:

;; Given a strand of DNA you can make its transcribed RNA strand by
;; replacing each DNA nucleotide with its complement:
;;
;; DNA    RNA
;; G      C
;; C      G
;; T      A
;; A      U

;;; Code

(defun troi/nucleotide-complement (n)
  "Given a nucleotide N (A, C, T, or G) return its RNA complement."
  (cond
   ((= ?G n) "C")
   ((= ?C n) "G")
   ((= ?T n) "A")
   ((= ?A n) "U")
   (t (error "Unknown DNA nucleotide %c" n))))

(defun to-rna (strand)
  "Transcribe a DNA strand into its RNA complement via RNA transcription."
  (mapconcat 'troi/nucleotide-complement strand ""))

(provide 'rna-transcription)
;;; rna-transcription.el ends here
