;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;; Author: Troy Brumley <blametroi@gmail.com>

;;; Commentary:

;; Another exercism. Where are my bell, book, and candle?
;;
;; Interesting parts of this exercise.
;;
;; * Nested loops. This reads better when the actual test for
;;   each possible anagram is factored into a separate
;;   function.
;; * The dolist macro hides the details of iterating over the
;;   list of candidates and building the list of anagrams.

;;; Code:

;; helper to compare two words.
(defun troi-anagramp (word1 word2)
  "Are the two strings WORD1 and WORD2 anagrams?

A word is not its own anagram, regardless of case."
  (let ((dword1 (downcase word1))
        (dword2 (downcase word2)))
  (and (not (string= dword1 dword2))
       (= (length dword1) (length dword2))
       (let ((word1-norm (sort (string-to-list dword1) '<))
             (word2-norm (sort (string-to-list dword2) '<)))
         (equal word1-norm word2-norm)))))

;; assigned function to return list of actual anagrams.
(defun anagrams-for (subject candidates)
  "Are the members of the list CANDIDATES anagrams of SUBJECT?

Return a list of all the CANDIDATES that are anagrams of SUBJECT.
If none of the candidates are anagrams, an empty list is
returned.

A word is not its own anagram, regardless of case."
  (let (anagrams '())
    (dolist (candidate candidates anagrams) ;; dolist feels like cheating
      (and (troi-anagramp subject candidate)
           (setq anagrams (append anagrams (list candidate)))))))

(provide 'anagram)
;;; anagram.el ends here
