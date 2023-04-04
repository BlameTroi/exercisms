(import (rnrs))

;; sum the unique multiples of the elements of ints that are less than limit
;; ie, only count common multiples once

(define (sum-of-multiples ints limit)
  (letrec* ((multiples (make-vector (1+ limit) 0))
            ;; note that existence of multiple once in the tracking vector
            (f (lambda (x)
                 (do ((i x (+ i x)))
                     ((> i limit))
                   (if (= 0  (vector-ref multiples i))
                       (vector-set! multiples i i)))))
            ;; check multiples for each int sent
            (g (lambda (xs)
                 (cond ((null? xs) '())
                       (else
                        (if (> (car xs) 0)
                            (f (car xs)))
                        (g (cdr xs))))))
            ;; sum the multiples from the tracking vector
            (h (lambda ()
                 (let ((i 0) (s 0))
                   (while (< i limit)
                     (set! s (+ s (vector-ref multiples i)))
                     (set! i (1+ i)))
                   s))))
    (g ints) (h)))
