(import (rnrs))

(define (binary-search array target)
  "Find TARGET in vector ARRAY. Return its index or 'not-found."
  (cond ((zero? (vector-length array)) 'not-found)
        ((= 1 (vector-length array)) (if (= target (vector-ref array 0)) 0 'not-found))
        (else
         (let ( (l 0)
                (m 0)
                (h (1- (vector-length array)))
                (searching #t)
                (result 'not-found) )
           (while searching
             (set! m (+ l (round (/ (- h l) 2))))
             (cond ((= (vector-ref array m) target) (set! searching #f) (set! result m))
                   ((>= l h) (set! searching #f))
                   ((< target (vector-ref array m)) (set! h (1- m)))
                   ((> target (vector-ref array m)) (set! l (1+ m)))
                   (else (error 'binary-search "well i'm lost" array target))))
           result))))
