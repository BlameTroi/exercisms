(import (rnrs))

;; An Armstrong number is a number that is the sum of its
;; own digits each raised to the power of the number of
;; digits.
;;
;; Here are two different implementations, one iterative
;; and the other recursive.

(define (armstrong-number? n)
  "Is a number an Armstrong number?"
  (armstrong-recursive n))


(define (armstrong-recursive n)
  (letrec* ((ns (string->list (number->string n)))
            (nd (length ns))
            (ord-0 (char->integer #\0))
            (val (lambda (c) (- (char->integer c) ord-0)))
            (armstrong-sum
             (lambda (ns)
               (cond ((null? ns) 0)
                     (else (+ (expt (val (car ns)) nd)
                              (armstrong-sum (cdr ns))))))))
    (= (armstrong-sum ns) n)))


(define (armstrong-iterative n)
  (let* ((ord-0 (char->integer #\0))
         (ns (string->list (number->string n)))
         (nd (length ns))
         (s 0))
    (while (not (null? ns))
      (set! s (+ s (expt (- (char->integer (car ns)) ord-0) nd)))
      (set! ns (cdr ns)))
    (= s n)))
