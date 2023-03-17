(import (rnrs)
        (rnrs arithmetic bitwise))

;; clearly the intent is to use the bitwise support
;; to speed things up, but first let's just use a
;; vector ... and it's more than fast enough.
;;
;; after some reading, bitvectors seem to be the way
;; to go.
;;
;; and they are great in guile, but apparently they
;; don't exist in chez. trying a bytevector based
;; implementation which should still be a huge
;; memory and access time saving over an array.

(define (sieve n)
  "Use the sieve of Erotosthenes to find and return
the all the primes up to and including N.  Return a
list of the primes."

  (cond ((< n 2) '())
        (else
         (let ((flags (make-bits (+ n 2) 255))
               (i 2)
               (res '()))
           ;; mark non primes with #f
           (clear-bit-flag flags 0)
           (clear-bit-flag flags 1)
           (while (< i (1+ n))
             (strike-out-multiples flags i n)
             (set! i (next-prime flags (1+ i) n)))
           ;; convert prime flags to list of primes
           (set! i n)
           (while (> i 1)
             (if (test-bit-flag flags i)
                 (set! res (cons i res)))
             (set! i (1- i)))
           res))))


(define (strike-out-multiples  v i n)
  "Strike out (remove) entries from V, a vector of booleans
representing integers, starting from I for every Ith slot
up until N, but leaving I clear. In effect removing all
multiples of I from the vector."

  (let ((m i))
    (set! n (1+ n))
    (while (<= (+ i m) n)
      (set! i (+ i m))
      (clear-bit-flag v i))))


(define (next-prime v i n)
  "Find the next clear entry in V greater than or equal to I."

  (while (and (<= i n) (not (test-bit-flag v i)))
    (set! i (1+ i)))
  i)


;; Simple API for bitwise access to a bytevector (u8). Created for
;; Sieve of Erosothenes but might be useful elsewhere.
;;
;; import rnrs bitwise for Guile.
;;
;; Bits are numbered from 0 in byte 0 of the vector.
(define (make-bits n i)
  "Create a bytevector for u8 access to bit flags numbered 0
through N. Initialize contents to u8 integer I."
  (cond ((or (< n 1) (< i 0) (> i 255)) (error 'make-bits "illegal number of bits or mask" n i))
        (else (make-bytevector (ash (+ 9 n) -3) i)))) ;; 8 bits to byte with some padding

(define (byte-for-bit n)
  "Returns the number of the byte in a bytevector that holds
bit N."
  (ash n -3))

(define (mask-for n)
  "Given a bit within the byte holding N, provide a mask to
isolate it."
  (ash 1 (bitwise-and n 7)))

(define (test-bit-flag v n)
  "Test single bit N in the bits byte vector V (created by
make-bits). Returns a boolean."
  (= (mask-for n) (bitwise-and (bytevector-u8-ref v (byte-for-bit n)) (mask-for n))))

(define (set-bit-flag v n)
  "Set the single bit N in the bits byte vector V to 1."
  (bytevector-u8-set! v (byte-for-bit n) (bitwise-ior (bytevector-u8-ref v (byte-for-bit n)) (mask-for n))))

(define (clear-bit-flag v n)
  "Set the single bit N in the bits byte vector V to 0."
  (bytevector-u8-set! v (byte-for-bit n) (bitwise-and (bytevector-u8-ref v (byte-for-bit n)) (- 255 (mask-for n)))))
