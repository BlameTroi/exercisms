(define (accumulate f xs)
  "Roll your own map function, applying function F to every element
of list XS."
  (cond
   ((null? xs) xs)
   (else (cons (apply f (list (car xs))) (accumulate f (cdr xs))))))
