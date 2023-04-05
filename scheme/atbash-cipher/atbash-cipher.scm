(import (rnrs))

;; More than a little overkill here since this can be done with
;; straight calculations instead of table lookups, but I wanted
;; to play with some things: lazy initialization, vectors.

(define (normalize s)
  "Remove characters that don't go through the encoder."
  (letrec* ((f (lambda (c) (or (and (char>=? c #\0) (char<=? c #\9))
                               (and (char>=? c #\a) (char<=? c #\z))
                               (and (char>=? c #\A) (char<=? c #\Z))))))
    (list->string (filter f (string->list s)))))

;; 'constants'

(define ord-a (char->integer #\a))
(define ord-z (char->integer #\z))
(define +falpha+ '())
(define +balpha+ '())

;; alphabet forward helpers

(define (findex c)
  "Index of clear text character into encoded text alphabet."
  (- (char->integer c) ord-a))

(define (falpha)
  "Returns clear text alphabet. Lazy initialization."
  (if (null? +falpha+)
      (begin
        (set! +falpha+ (make-vector 26 #\nul))
        (do ((c 0 (1+ c)))
            ((>= c 26))
          (vector-set! +falpha+ c (integer->char (+ ord-a c))))))
  +falpha+)

;; alphabet backward helpers
(define (bindex c)
  "Index of encoded text character into clear text alphabet."
  (- ord-z (char->integer c)))

(define (balpha)
  "Returns encoded text alphabet. Lazy initialization."
  (if (null? +balpha+)
      (begin
        (set! +balpha+ (make-vector 26 #\nul))
        (do ((c 0 (1+ c)))
            ((>= c 26))
          (vector-set! +balpha+ c (integer->char (- ord-z c))))))
  +balpha+)

;; single character encode and decode

(define (enchar c)
  "Encode a single character, returns nul character if invalid input."
  (cond ((and (char>=? c #\0) (char<=? c #\9)) c)
        ((and (char>=? c #\A) (char<=? c #\Z)) (enchar (char-downcase c)))
        ((and (char>=? c #\a) (char<=? c #\z))
         (vector-ref (balpha) (findex c)))
        (else #\nul)))

(define (dechar c)
  "Decode a single character, returns nul character if
 invalid input."
  (cond ((and (char>=? c #\0) (char<=? c #\9)) c)
        ((and (char>=? c #\A) (char<=? c #\Z)) (dechar (char-downcase c)))
        ((and (char>=? c #\a) (char<=? c #\z))
         (vector-ref (falpha) (bindex c)))
        (else #\nul)))

;; formatting

(define (by-fives xs)
  "Break a list of encoded characters into chunks of five
 separated by spaces."
  (let ((i 0) (pending #f) (res '()))
    (while (not (null? xs))
      (if pending
          (set! res (cons #\space res)))
      (set! pending #f)
      (set! res (cons (car xs) res))
      (set! xs (cdr xs))
      (set! i (1+ i))
      (if (= i 5)
          (begin
            (set! i 0)
            (set! pending #t))))
    (reverse res)))

;; the encoder main

(define (encode phrase)
  "Encode string PHRASE via atbash algorithm."
  (list->string (by-fives (map enchar (string->list (normalize phrase))))))

;; the decodder main
(define (decode phrase)
  "Decode string PHRASE via atbash algorithm."
  (list->string (map dechar (string->list (normalize phrase)))))
