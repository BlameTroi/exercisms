(import (rnrs))

;; the basic way to do this is by mapping the conversion over
;; each individual character in the string. from there it's
;; just a question of should there be one or two visible
;; functions involved. i find factoring out the character
;; complement more readable, but either works.

;; submitted using the second approach, but either works in the
;; downloaded tests.

;; approach 1

;; complement individual character
;; returns "?" for any unknown dna character
(define (dna-character->rna-character dna)
  (cond
   ((equal? dna #\A) '#\U)
   ((equal? dna #\T) '#\A)
   ((equal? dna #\G) '#\C)
   ((equal? dna #\C) '#\G)
   ('(#t) '#\?)))

;; complement string being a bit more readable
(define (alternate-dna->rna dna)
  (list->string (map dna-character->rna-character (string->list dna))))

;; approach 2

;; complement string in a single defined function using a closure
(define (dna->rna dna)
  (list->string (map (lambda (x)
                       (cond
                        ((equal? x #\A) '#\U)
                        ((equal? x #\T) '#\A)
                        ((equal? x #\G) '#\C)
                        ((equal? x #\C) '#\G)
                        ('(#t) '#\?)))
                     (string->list dna))))
