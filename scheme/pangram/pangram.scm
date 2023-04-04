(import (rnrs))

(define (pangram? phrase)
  "Is string PHRASE a pangram? This is case insensitive."
  (letrec*
      ((full (string->list (string-downcase phrase)))
       (letters-only (filter (lambda (c) (and (char>=? c #\a) (char<=? c #\z))) full))
       (only-one (lambda (cs accum)
                   (cond
                    ((null? cs) accum)
                    (else
                     (only-one (cdr cs) (if (not (member (car cs) accum))
                                            (cons (car  cs) accum)
                                            accum)))))))
    (= 26 (length (only-one letters-only '())))))
