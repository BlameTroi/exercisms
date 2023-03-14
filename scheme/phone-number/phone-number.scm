(import (rnrs))
(use-modules (srfi srfi-1))

;; for NANP valid phone numbers are formatted
;; area code - exchange - subscriber number
;;
;; (NXX)-NXX-XXXX
;;
;; N .. 2->9
;; X .. 0->9
;;
;; NXXNXXXXXX
;; 0123456789
;;
;; So positions 0 and 3 must be 2-9, instead of 0-9.
;;
;; They may be preceeded by a country code of
;; 1 but it is not required.
;;
;; remove punctuation and validate per above
;; rules, return clean 10 digit number or
;; error.

(define (clean x)
  "Given a string containing a phone number with
various and probably inconsistent punctuation,
clean it and return a valid NANP 10 digit phone
number. A country code of 1 might be prefixed
on input, but should not be returned."

  (cond ((or (null? x) (not (string? x)) (string= "" x))
         (error 'clean "invalid type or missing argument" x))
        (else
         (let ((d (string-numeric-only x)))
           (if (string= (substring d 0 1) "1")
               (set! d (substring d 1)))
           (cond ((not (= (string-length d) 10))
                  (error 'clean "phone number must be 10 or 11 digits" x d))
                 ((string< (substring d 0) "2")
                  (error 'clean "phone number area code must begin with 2-9" x d (substring d 0)))
                 ((string< (substring d 3) "2")
                  (error 'clean "phone number exchange must begin with 2-9" x d (substring d 3)))
                 (else d))))))

(define (string-numeric-only x)
  "Given string X, return a string containing any
decimal digits found."
  (list->string (filter (lambda (x) (char-numeric? x)) (string->list x))))
