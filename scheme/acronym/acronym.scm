(import (rnrs))

(define (acronym test)
  "Generate an acronym from string TEST."

  (define (scrub s)
    "Replace unwanted characters in s with blanks."
    (list->string (map (lambda (c)
                         (cond
                          ((char=? #\- c) #\space)
                          ((char=? #\_ c) #\space)
                          (else c)))
                       (string->list s))))

  (define (each-initial xs accum)
    "Return the first letter of each word until end of xs."
    (cond
     ((null? xs) accum)
     (else (each-initial
            (cdr xs)
            (if (string= "" (car xs))
                accum
                (string-append accum (substring (car xs) 0 1)))))))

  (each-initial
   (filter (lambda (s) (string<> "" s))
           (string-split (scrub (string-upcase test)) #\space)) ""))
