(import (rnrs))

(define (collatz n)
  "Given a number N, how many steps does it take to reach 1
following the steps of the Collatz Conjecture? Input is not
validated."
  (letrec ((f (lambda (x s)
                (cond ((= 1 x) s)
                      (else (f (if (even? x)
                                   (ash x -1) ; n = n / 2
                                   (+ 1 x x x)) ; n = 3n + 1
                               (1+ s)))))))
    (f n 0)))
