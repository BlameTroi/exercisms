(import (rnrs))
(use-modules (srfi srfi-1))


(define (factorize n)
  "Given number N return a list of its prime factors."
  (let ((pf 2) (x n) (res '()))
    (while (> x 1)
      (if (zero? (remainder x pf))
          (begin (set! res (cons pf res))
                 (set! x (/ x pf)))
          (set! pf (next-prime (1+ pf)))))
    (reverse res)))


(define (next-prime n)
  "Find the next possible prime greater than OR EQUAL TO a number N."
  (let ((p n))
    (while (not (prime? p))
      (set! p (+ p 1)))
    p))


(define (prime? n)
  "Is N prime? Use the 6k+/-1 optimization."
  (cond ;; yeah, some of the cond clauses are silly
        ((= n 1)              #f)
        ((or (= n 2) (= n 3)
             (= n 5) (= n 7)) #t)
        ((< n 11)             #f)
        ((even? n)            #f)
        ((zero? (remainder n 3)) #f)
        (else
         (let ((i 5) (lim (sqrt n)) (res #t))
           (while (and res (< i lim))
             (if (or (zero? (remainder n i)) (zero? (remainder n (+ 2 i))))
                 (set! res #f)
                 (set! i (+ i 6))))
           res))))

