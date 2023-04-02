(import (rnrs))

;; constants
(define tri-digits '(#\0 #\1 #\2))
(define tri-power 3)
(define ord-0 (char->integer #\0))

;; helpers
(define (only? s cs)
  "Does string S contain only characters from the list cs?"
  (letrec* ((f (lambda (xs) (cond ((null? xs) #t)
                                  ((not (member (car xs) cs)) #f )
                                  (else (f (cdr xs)))))))
    (f (string->list s))))

;; solution convert trinary string to decimal number
(define (to-decimal s)
  "Convert the number in string S from trinary (base 3) to
decimal. S may only contain the digits 0, 1, and 2."
  (cond ((not (only? s tri-digits)) 0)
        (else (let ((p 0) (res 0) (d #\0) (ds (reverse (string->list s))))
                (while (not (null? ds))
                  (set! d (- (char->integer (car ds)) ord-0))
                  (set! ds (cdr ds))
                  (set! res (+ res (* d (expt tri-power p))))
                  (set! p (1+ p)))
                res))))
