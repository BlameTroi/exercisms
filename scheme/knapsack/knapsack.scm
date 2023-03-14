(import (rnrs))
(use-modules (srfi srfi-1))

(define (max-value-min-weight? a b)
  "Sort predicate to order a list of ((weight value)) so that
more valuable items sort first, but use least weight as a tie
breaker."

  (cond ((> (cadr a) (cadr b)) #t)
        ((= (cadr a) (cadr b)) (< (car a) (car b)))
        (else #f)))

(define (max-weight-max-value? a b)
  "Sort predicate to order a list of ((weight value)) so that
heavier items and higher value items sort first."

  (cond ((> (car a) (car b)) #t)
        ((= (car a) (car b)) (> (cadr a) (cadr b)))
        (else #f)))

(define (most-dense? a b)
  "Sort predicate to order a list of ((weight value)) so that
the most dense (high value per unit of weight) sort first."
  (let ((density-a (/ (cadr a) (car a)))
        (density-b (/ (cadr b) (car b))))
    (> density-a density-b)))

(define (least-dense? a b)
  "Sort predicate to order a list of ((weight value)) so that
the least dense (high value per unit of weight) sort first."
  (let ((density-a (/ (cadr a) (car a)))
        (density-b (/ (cadr b) (car b))))
    (< density-a density-b)))

(define (try-this-fit capacity weights-values)
  "items are fed to us in an order, check it out"
  (let ((result 0) (avail capacity))
    (while (not (null? weights-values))
      (if (<= (car (car weights-values)) avail)
          (begin (set! avail (- avail (car (car weights-values))))
                 (set! result (+ result (cadr (car weights-values))))))
      (set! weights-values (cdr weights-values)))
    result))

(define (outside-in-simple capacity weights-values)
  "work from the ends."
  (cond ((null? weights-values) 0)
        ((< capacity (car (car weights-values)))
         (outside-in-simple capacity (reverse (cdr weights-values))))
        (else
         (+ (cadr (car weights-values))
            (outside-in-simple (- capacity (car (car weights-values)))
                               (reverse (cdr weights-values)))))))


(define (tester capacity weights values)
  "A simple driver for cut and paste testing from the test input. Just
run everything and display result."
  (let ((unsorted 0) (reversed 0)
        (sort1 0) (sort2 0)
        (sort3 0) (sort4 0)
        (zipped (zip weights values)))
    (set! unsorted (outside-in-simple capacity zipped))
    (set! reversed (outside-in-simple capacity (reverse zipped)))
    (set! sort1 (outside-in-simple capacity (sort zipped max-weight-max-value?)))
    (set! sort2 (outside-in-simple capacity (sort zipped max-value-min-weight?)))
    (set! sort3 (outside-in-simple capacity (sort zipped most-dense?)))
    (set! sort4 (outside-in-simple capacity (sort zipped least-dense?)))
    (display unsorted)(display " ")(display reversed)(display " ")
    (display sort1)(display " ")(display sort2)(display " ")
    (display sort3)(display " ")(display sort4)(display " ")
    (newline)))

(define (knapsack capacity weights values)
  "Fit items into a knapsack of CAPACITY to maximize VALUEs allowing
for item WEIGHTs. You can only take one of each item. Returns the
total maximum value that can fit into the knapsack."
  (let ((zipped (zip weights values)))
    (cond
     ;;;
     ;;; deal with special cases first
     ;;;
     ;; pack has no capacity
     ((< capacity 1) 0)
     ;; nothing to fit
     ((null? weights) 0)
     ;; will nothing fit?
     ((< capacity (apply min weights)) 0)
     ;; will it all fit?
     ((>= capacity (apply + weights)) (apply + values))
     ;;;
     ;;; more serious attempts
     ;;;
     ;; Obvious first attempts are to sort the weights-values in
     ;; various ways and then keep stuffing until we hit the end
     ;; of the list or the pack won't hold any more. Simplistic
     ;; and not likely to work, but how bad are the results?
     ;;
     ;; Interestingly enough, no one of the first six orders is
     ;; consistently better than another with the test data
     ;; provided. I'd say that's a tribute to good random data.
     ;;
     ;; return the max of our various attempts.
     (else
      (max
       ;; it's all gotta be better than this
       -1
       ;; These 'try-this-fit' are attempts at a try sorting and then
       ;; filling in order approach until the end of items is reached
       (try-this-fit capacity zipped)
       (try-this-fit capacity (reverse zipped))
       (try-this-fit capacity (sort zipped most-dense?))
       (try-this-fit capacity (sort zipped least-dense?))
       (try-this-fit capacity (sort zipped max-weight-max-value?))
       (try-this-fit capacity (sort zipped max-value-min-weight?))
       ;; and now work from outside in
       (outside-in-simple capacity zipped)
       (outside-in-simple capacity (reverse zipped))
       (outside-in-simple capacity (sort zipped most-dense?))
       (outside-in-simple capacity (sort zipped least-dense?))
       (outside-in-simple capacity (sort zipped max-weight-max-value?))
       (outside-in-simple capacity (sort zipped max-value-min-weight?))
       ;;
       )))))
