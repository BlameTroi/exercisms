(import (rnrs))

(define (square-of-sum n)
  "Square the sum of numbers from 1 to N."
  (let ((s (/ (* n (1+ n)) 2)))
    (* s s)))

(define (sum-of-squares n)
  "Sum the squares of numbers from 1 to N."
  (cond ((zero? n) 0)
         (else (+ (* n n) (sum-of-squares (1- n))))))

(define (difference-of-squares n)
  "Difference between the square of the sum of 1 to N and
sum of the square of 1 to N."
  (- (square-of-sum n) (sum-of-squares n)))
