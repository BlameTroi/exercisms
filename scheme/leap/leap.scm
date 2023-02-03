(import (rnrs))

(define (leap-year? year)
  "Given a year, is it a leap year?
A leap year is defined in the Gregoarian calendar as:
* a year evenly divisible by 4
* that is not evenly divisible by 100
* unless it is evenly divisible by 400"
  (and (or (= 0 (mod year 400))
           (not (= 0 (mod year 100))))
       (= 0 (mod year 4))))
