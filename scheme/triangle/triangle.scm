(import (rnrs))
;;
;; interestingly, (triangle 2 4 2) should error here but the
;; go version of the exercise allows it. There a flat line
;; is apparently ok.
;;
(define (triangle a b c)
  "Given the lengths of three possible sides, could they form
a triangle? If so, return the kind: equilateral (all sides
equal), isosceles (two sides equal), or scalene (no sides
equal). If the sides could not form a triangle, signal an
error."
  (cond ((or (<= a 0) (<= b 0) (<= c 0))
         (error 'triangle "illegal side length, all must be greater than 0"))
        ((and (= a b) (= b c))
         'equilateral)
        ;; (a+b) < c || (a+c) < b || (b+c) < a
        ((or (<= (+ a b) c) (<= (+ a c) b) (<= (+ b c) a))
         (error 'triangle "impossible configuration of sides"))
        ((or (= a b) (= a c) (= b c))
         'isosceles)
        (else
         'scalene)))
