;;; word-count.el --- word-count Exercise (exercism)
;;; word-count.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun word-count (words)
  "Given a string of WORDS, return an associative list of word:count.
The count is case insensitive, results are unordered, and all punctuation
other than an apostrophe for contraction are ignored. Words are whitespace
delimited."
  (interactive "sUse your words: ")
  (if (stringp words)
      (let ((lower) (downcase words))
         h)))





(provide 'word-count)
;;; word-count.el ends here
