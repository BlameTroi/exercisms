(import (rnrs))

(define (convert number)

  (define (factor? n f)
    (zero? (remainder n f)))

  (define (concat xs accum)
    (cond ((null? xs) accum)
          (else (concat (cdr xs) (string-append accum (car xs))))))

  (let ((res '()))
    (if (factor? number 7) (set! res (cons "Plong" res)))
    (if (factor? number 5) (set! res (cons "Plang" res)))
    (if (factor? number 3) (set! res (cons "Pling" res)))
    (if (null? res)
        (number->string number)
        (concat res ""))))
