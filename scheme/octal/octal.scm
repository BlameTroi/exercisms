(import (rnrs))
(use-modules (srfi srfi-1))

(define (to-decimal s)
  "Well that's poorly named. Convert string S of octal digits to
an integer. It's gonna look decimal, but it's probably binary
inside."
  (cond ((or (null? s) (not (string? s)) (string= "" s)) 0)
        (else  (let ((r 0) (xs (string->list s)))
                 (while (not (null? xs))
                   (if (or (char<? (car xs) #\0) (char>? (car xs) #\7))
                       (begin (set! r 0) (set! xs '())) ;; (char->integer #\0) is 48
                       (begin (set! r (+ (* r 8) (- (char->integer (car xs)) 48))) (set! xs (cdr xs)))))
                 r))))
