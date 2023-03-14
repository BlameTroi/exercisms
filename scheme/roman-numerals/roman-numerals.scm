(import (rnrs))
(use-modules (srfi srfi-1))

;; An exercism to convert a given integer from 0 to 3999 into
;; Roman Numeral notation.
;;
;; Important things to remember about our Roman ancestors:
;; * They got shit done.
;; * They hadn't found a need for ZERO.

(define (roman n)
  "Convert integer N to a Roman Numeral string. While not
specified in the problem statement, legal range for N is
from 1 to 3999."

  (cond ((or (< n 1) (> n 3999)) (error 'roman "illegal range, must be 1-3999" n))
        (else
         ;; use digits as index into the mapping table and
         ;; combine the values to create a Roman Numeral
         ;; string. This works best from right to left.
         (let* ((svalue (string-reverse (number->string n)))
                (power 0)
                (roman "")
                (i 0)
                (d 0))
           (while (< i (string-length svalue))
             (set! d (string->number (substring svalue i (1+ i))))
             (set! roman (string-append (vector-ref digit-table (+ power d)) roman))
             (set! power (+ power 10))
             (set! i (1+ i)))
           roman))))

(define digit-table
  ;; a mapping between powers of ten and Roman Numerals. Legal
  ;; values range from 1 through 3999. Empty strings represent
  ;; illegal values. Romans didn't do zero, etc.
  (list->vector                                        ;; powers of ten
   '("" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" ;; 0 - units
     "" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC" ;; 1 - tens
     "" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM" ;; 2 - hundreds
     "" "M" "MM" "MMM" "" "" "" "" "" "")))            ;; 3 - thousands
