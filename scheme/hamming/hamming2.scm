(import (rnrs))

(define (strand-differences strand1 strand2)
  (map (lambda (a b) (if (char=? a b) 0 1))
       (string->list strand1)
       (string->list strand2)))


(define (hamming-distance strand1 strand2)
  (apply + (strand-differences strand1 strand2)))
