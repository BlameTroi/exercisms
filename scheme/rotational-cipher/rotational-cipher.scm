(import (rnrs))

(define (rotate phrase dx)

  (define ord-a (char->integer #\a))
  (define ord-z (char->integer #\z))
  (define ord-A (char->integer #\A))
  (define ord-Z (char->integer #\Z))

  (define (rot c)
    (modulo (+ c dx) 26))

  (define (enc c)
    (cond ((and (char>=? c #\a) (char<=? c #\z)) (integer->char (+ ord-a (rot (- (char->integer c) ord-a)))))
          ((and (char>=? c #\A) (char<=? c #\Z)) (integer->char (+ ord-A (rot (- (char->integer c) ord-A)))))
          (else c)))

  (list->string (map enc (string->list phrase))))
