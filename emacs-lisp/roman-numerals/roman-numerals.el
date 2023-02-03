;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;; Author: Troy Brumley <blametroi@gmail.com>

;;; Commentary:

;; An exercism to convert a given integer from 0 to 3999 into
;; Roman Numeral notation.

;; Important things to remember about our Roman ancestors:
;; * They got shit done.
;; * They hadn't found a need for ZERO.

;;; Code:

(defconst troi/digit-table
  ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"
   "" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"
   "" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"
   "" "M" "MM" "MMM" "" "" "" "" "" ""]
  "troi/digit-table provides a mapping between powers of ten
and Roman Numerals. Legal values are from 1 to 3999. Empty strings
repreent illegal Roman Numeral values.")

(defconst troi/ord0
  (string-to-char "0")
  "The ordinal of the character '0'. It's 48 in most character
sets, but you never know.")

(defun to-roman (value)
  "Given an integer VALUE (1-3999) return its equivalent as a
string of roman numerials."
;; use digits as index into the mapping
;; table and combine the values to create
;; the roman numeral string.
  (let* ((svalue (reverse (number-to-string value)))
         (power 0)
         (roman "")
         (i 0)
         (d 0))
    (while (< i (length svalue))
      (setq d (- (aref svalue i) troi/ord0))
      (setq roman (concat
                   (aref troi/digit-table (+ power d))
                   roman))
      (setq power (+ power 10))
      (setq i (+ i 1)))
    roman))

;; testing ..
;; (reverse "1234")
;; (- (aref "1" 0) troi/ord0)
;; (aref troi/digit-table 1)
;; (to-roman 1)
;; (to-roman 2)
;; (to-roman 9)
;; (to-roman 10)
;; (to-roman 20)
;; (to-roman 50)
;; (to-roman 100)
;; (to-roman 1024)

(provide 'roman-numerals)
;;; roman-numerals.el ends here
