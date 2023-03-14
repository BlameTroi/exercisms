(import (rnrs))
(use-modules (srfi srfi-1))

(define (white? c)
  "White space predicate for 'split-string'."
  (or (equal? c #\space) (equal? c #\newline) (equal? c #\tab)))

(define (punct? c)
  "Punctuation predicate for 'split-string'. Apostrophes require
special handling. This is way overboard and instead filtering
to include only alphabetics, numerics, and apostrophes would likely
be better, but this works for now."
  (or (equal? c #\.) (equal? c #\!) (equal? c #\?) (equal? c #\,)
      (equal? c #\:) (equal? c #\;) (equal? c #\-) (equal? c #\+)
      (equal? c #\") (equal? c #\*) (equal? c #\\) (equal? c #\_)
      (equal? c #\@) (equal? c #\#) (equal? c #\$) (equal? c #\%)
      (equal? c #\^) (equal? c #\&) (equal? c #\() (equal? c #\))
      (equal? c #\[) (equal? c #\]) (equal? c #\{) (equal? c #\})
      (equal? c #\<) (equal? c #\>)))

(define (splitter? c)
  "Splitting characters for the word-count problem."
  (or (white? c) (punct? c)))

(define (scrub w)
  "Scrub the word W, a string. Downcase and remove any quoting
apostrophes."
  (let ((d (string-downcase w)))
    (cond ((not (string-index d #\')) d)
          ((= 0 (string-index d #\')) (scrub (substring d 1)))
          ((= (1- (string-length d)) (string-rindex d #\')) (scrub (substring d 0 (1- (string-length d)))))
          (else d))))

(define (squeeze xs)
  "List XS contains words sorted character insensitively. Returns a
list of pairs of (word . count) where count is the number of times
the word was seen."
  (letrec ((f (lambda (p xs accum)
                (cond ((null? xs) (cons p accum))
                      ((string= (car p) (car xs)) (f (cons (car p) (1+ (cdr p))) (cdr xs) accum))
                      (else (f (cons (car xs) 1)  (cdr xs) (cons p accum)))))))
    (f (cons (car xs) 1) (cdr xs) '())))

(define (not-empty? x)
  "Predicate to remove empty strings, "", from a list of strings."
  (not (string= "" x)))

(define (word-count sentence)
  "Given a sentence, return a count of words in the sentence. Words
can be:
1. A _number_ composed of one or more ASCII digits
   (ie 0 or 1234)
2. A _simple word_ composed of one or more ASCII letters
   (ie a or they)
3. A _contraction_ of two _simple words_ joined by a single apostrophe
   (ie it's or they're)
Case insensitive, order insensitive, white space rules are liberal.
Returns a list of (word . count) pairs."

  (squeeze
   (sort
    (map scrub (filter not-empty? (string-split (string-downcase sentence) splitter?)))
    string<)))
