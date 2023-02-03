(import (rnrs))


(define (list-hamming accum l1 l2)
  "Return the hamming distance of two lists.
This is an internal function and does not
validate its input."
  (if (null? l1)
      '(accum)
      (let ()
        ([new-accum (+ accum (if (not (equal? (car l1) (car l2)))
                                 1
                                 0))])
        (list-hamming new-accum (cdr l1) (cdr l2)))))


(define (hamming-distance strand-a strand-b)
  "Return the hamming distance of two strings.
The strings must be of equal length or an error is
returned."
  (let ([la (string->list strand-a)]
        [lb (string->list strand-b)])
    (if (not (= (length la) (length lb)))
        (raise "hamming-distance lengths must be equal")
        (list-hamming 0 la lb))))
