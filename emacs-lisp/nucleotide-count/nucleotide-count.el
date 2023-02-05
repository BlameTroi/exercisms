;;; nucleotide-count.el --- nucleotide-count Exercise (exercism)  -*- lexical-binding: t; -*-

;; Author: Troy Brumley <blametroi@gmail.com>

;;; Commentary:

;; A variation on word-count that shows a few interesting things.
;;
;; * I opted for dolist but mapc or mapcar could be used here.
;; * A literal alist in source can easily be modified and cause
;;   what appear to be heisenbugs. This makes perfect sense once
;;   you remember that data is code and code is data in lisp,
;;   but it runs counter to the source-compile-execute reflexes.
;; * Many optional parameters (I'm looking at you, alist-get) are
;;   truly optional. I need to stop overspecifying, it's not
;;   idiomatic.

;;; Code:

(defun nucleotide-count (dnaseq)
  "Returns an alist of the nucleotide counts in the string DNASEQ.
Returns error if a nucleotide code other than A C T or G is found."
  (if (string-match-p "[^GCTA]" dnaseq)
      (error "%s" "DNA sequence given contains an invalid nucleotide")
    (let ((counts (copy-alist '((?A . 0) (?C . 0) (?G . 0) (?T . 0))))
          (fragmented (string-to-list dnaseq)))
       (dolist (nucleotide fragmented counts)
         (setq new-count (+ 1 (alist-get nucleotide counts)))
         (setf (alist-get nucleotide counts) new-count))
         counts)))

(provide 'nucleotide-count)
;;; nucleotide-count.el ends here
