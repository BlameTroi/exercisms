;;; word-count.el --- word-count Exercise (exercism)

;;; Commentary:

;; Count 'words' in a string. Words are case insensitive; apostrophe for contraction
;; does not break a word; sequences of digits are words; punctuation is otherwise
;; ignored.

;; Interesting bits tripped over while doing this:
;;
;; * quoting function names when they are passed as parameters
;; * dolist
;; * building alist
;; * ended up not needing, but quasi-quote to get the value of a variable
;; * describe-function: use it!

;; TODO:
;;
;; * Better hanlding of apostrophes.
;; * Factor out the alist-get.

;;; Code:

(defun troi/string-clean (s)
  "Filter/clean/normalize the input string S for use by word-count.
Case must be insensitive, and only letters, decimal digits, and the
apostrophe for contraction are allowed.

BUG: Apostrophes as single quotes can slip through."
  (replace-regexp-in-string
   "[^a-z0-9\\' ]"
   ""
   (string-clean-whitespace (downcase s)))
  )


(defun word-count (s)
  "Returns an associated list of word-occurs pairs found in string S,
where words are case insensitive. Punctuation and special characters
are ignored except in the case of a contraction or some posessives.

BUG: Apostrophe handling isn't complete."
  (let (pairs
        (words (split-string (troi/string-clean s))))
    (dolist (word words pairs)
      (setq new-count (+ 1 (alist-get word pairs 0 nil 'string=)))
      (setf (alist-get word pairs 0 nil 'string=) new-count)
      pairs)))

(provide 'word-count)
;;; word-count.el ends here
