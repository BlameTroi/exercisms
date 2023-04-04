(import (rnrs))

(define (classify n)
  "Classify a natural number N as 'perfect, 'abundant, or
'deficient based on it's Aliquot sum."
  (cond ((or (< n 1) (inexact? n)) (error 'classify "must be a natural number" n))
        (else (letrec* ((sum 0) (lim (quotient n 2))
                        (f (lambda (i s)
                             (cond ((> i lim) s)
                                   (else (f (1+ i) (+ s (if (zero? (remainder n i)) i 0))))))))
                (set! sum (f 1 0))
                (cond ((= sum n) 'perfect)
                      ((> sum n) 'abundant)
                      (else      'deficient))))))
