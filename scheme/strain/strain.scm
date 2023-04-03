(import (rnrs))

(define (keep pred seq)
  (letrec* ((f (lambda (xs accum)
                 (cond
                  ((null? xs) (reverse accum))
                  ((apply pred (list (car xs))) (f (cdr xs) (cons (car xs) accum)))
                  (else (f (cdr xs) accum))))))
    (f seq '())))

(define (discard pred seq)
  (letrec* ((f (lambda (xs accum)
                 (cond
                  ((null? xs) (reverse accum))
                  ((not (apply pred (list (car xs)))) (f (cdr xs) (cons (car xs) accum)))
                  (else (f (cdr xs) accum))))))
    (f seq '())))
