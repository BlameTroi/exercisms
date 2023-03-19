(import (rnrs))

;; Encode and decode via an affine cypher using the letters a-z (case
;; insensitive). Numbers are passed in the clear and all other characters
;; are ignored.
;;
;; Encoding:
;;
;; `E(x) = (ax + b) mod m`
;;
;; - `x` is the letter's index from 0 - length of alphabet - 1
;; - `m` is the length of the alphabet. For the roman alphabet `m == 26`.
;; - and `a` and `b` make the key
;; - decimal digts are passed in the clear.
;; - `a` and `m` must be coprimes.
;;
;; Decoding:
;;
;; `D(y) = a^-1(y - b) mod m`
;;
;; - `y` is the numeric value of an encrypted letter, ie. `y = E(x)`
;; - it is important to note that `a^-1` is the modular multiplicative inverse
;;   (MMI) of `a mod m`
;; - the modular multiplicative inverse of `a` only exists if `a` and `m` are
;;   coprime.
;;
;; Modular Multiplicative Inverse:
;;
;; To find the MMI of `a`:
;; Find a value of n where  `an mod m = 1`


(define (encode key text)
  "Encodes string TEXT via affine cypher using KEY pair."

  (validate-key-pair key)
  (list->string (cypher-text 5 (affine-encoder
                                key
                                (string->list (string-downcase text))))))


(define (decode key text)
  "Decodes string TEXT via affine cypher using KEY pair."

  (validate-key-pair key)
  (list->string (affine-decoder
                 key
                 (string->list (string-downcase text)))))


;; The alphabet could be handled as a range of integers or characters
;; but providing it as a list allows for scrambling and the use of
;; other codepoints besides the limited Roman alphabet.
(define affine-alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h
                          #\i #\j #\k #\l #\m #\n #\o #\p
                          #\q #\r #\s #\t #\u #\v #\w #\x
                          #\y #\z))

;; Length of the alphabet, a global 'variable' used as a constant
;; throughout.
(define m (length affine-alphabet))


(define (affine-encoder key xs)
  "Encode a list of characters XS via the affine cypher using A and
B as keys. Decimal digits are passed unaltered. Alphabetic characters
in the affine-alphabet are encoded. All other characters are dropped."
  (let*
      ((a (car key)) ;; key part 1
       (b (cdr key)) ;; key part 2
       (r '())       ;; encoded xs
       (x   0)       ;; position of encoded character in alphabet
       (e   0)       ;; enocded c
       (c #\space))  ;; current character from xs
    ;; a and b form the key
    ;; e(x) = (ax + b) mod m
    (while (not (null? xs))
      (set! c (car xs))
      (set! xs (cdr xs))
      (set! x (index-into c affine-alphabet))
      (if (> x -1) ;; is character in alphabet?
          (begin
            (set! e (modulo (+ (* a x) b) m))
            (set! r (cons (character-at e affine-alphabet) r)))
          (if (and (char>=? c #\0) (char<=? c #\9))
              (set! r (cons c r)))))
    (reverse r)))


(define (affine-decoder key xs)
  "Decode a list of characters XS via the affine cypher using A and
B as keys. Decimal digits are passed unaltered. Alphabetic characters
in the affine-alphabet are decoded. All other characters are dropped."
  (let*
      ((a (car key)) ;; key part 1
       (b (cdr key)) ;; key part 2
       (r '())       ;; decoded xs
       (n (mmi a m)) ;; a^-1 in forumula below
       (y  0)        ;; position of c in alphabet
       (d  0)        ;; decoded y
       (c #\space))  ;; current character from xs
    ;; a and b form the key
    ;; d(y) = a^-1(y - b) mod m
    ;; a^-1 is the MMI of a mod m
    (while (not (null? xs))
      (set! c (car xs))
      (set! xs (cdr xs))
      (set! y (index-into c affine-alphabet))
      (if (> y -1) ;; is character in alphabet?
          (begin
            ;; to be provided
            (set! d (modulo (* n (- y b)) m))
            (set! r (cons (character-at d affine-alphabet) r)))
          (if (and (char>=? c #\0) (char<=? c #\9))
              (set! r (cons c r)))))
    (reverse r)))


(define (validate-key-pair key)
  "Is the first part of the KEY pair valid with regard to alphabet length
M? If not throw an error back."
  (if (or (not (pair? key)) (not (coprime? (car key) m)))
      (error 'encode "invalid key provided, part 1 must be coprime with m" key m)))


(define (cypher-text x xs)
  "Insert a character space every X elements into XS. Typically this
would be done for encrypted or decrypted text."
  (cond ((null? xs) xs)
        ((< x 1) xs)
        (else
         (let ((r '()) (tic x))
           (while (not (null? xs))
             (set! r (cons (car xs) r))
             (set! tic (1- tic))
             (set! xs (cdr xs))
             (if (and (not (null? xs)) (< tic 1))
                 (begin (set! r (cons #\space r))
                        (set! tic x))))
           (reverse r)))))


(define (index-into x xs)
  "Returns position relative to 0 of X in list XS or -1 if not found.
Implementing to use a list allows for discontiguous and/or shuffled
character sets."
  (cond ((null? xs) -1)
        (else (let ((m (member x xs)))
                (if (list? m)
                    (- (length xs) (length m))
                    -1)))))


(define (character-at x xs)
  "Returns the element at position X relative to 0 in list XS or '()
if out of bounds."
  (cond ((null? xs) '())
        ((zero? x) (car xs))
        (else (character-at (1- x) (cdr xs)))))


;; coprimes are two numbers that have only 1 as a common
;; divisor.
(define (coprime? a b)
  "Are integers A and B coprime?"
  (cond ((or (not (integer? a)) (not (integer? b))) #f)
        ((or (zero? a) (zero? b))                   #f)
        (else
         (= (gcd a b) 1))))


;; modular mulitplicative inverse
(define (mmi a m)
  "Return the modular multiplicative inverse (N) of A and M in
the relation AN mod M = 1 using a simple iterative solution
for the small values of M expected."
  (let ((n 1))
    (while (not (= 1 (modulo (* a n) m)))
      (set! n (1+ n)))
    n))


;; (gcd 105 252) ==> 21
;; Guile Scheme provides a built in GCD, but if not
;; available there's always:
;; (define (gcd a b)
;;   "Find the GCD of integer numbers A and B."
;;   (let ((t b))
;;     (while (not (zero? b))
;;       (set! t b)
;;       (set! b (modulo a b))
;;       (set! a t))
;;     a))
