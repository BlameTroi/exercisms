(import (rnrs))

;; cases that cause problems
;;
;; amt   coins          best answer
;; (23 (1 4 15 20))    '(4 4 15)
;; (27 (4 5))          '(4 4 4 5 5 5)
;; (21 (2 5 10 20))    '(2 2 2 5 10)
;; (63 (1 5 10 21 25)) '(21 21 21)
;;

;; for ease of testing
(define uscoins '(1 5 10 25 50))        ; american coinage
(define lpcoins '(1 4 15 20 50))        ; lilliputian coinage
(define lecoins '(1 5 10 21 25))        ; lower elbonian coinage
(define nopenny '(5 15 25))             ; no units one
(define caliber '(4 5))                 ; no units two

;; general helpers

;; too cute dynamism for some handy functions
(define (applicator f xs)
  (cond ((null? xs) #f)
        ((not (list? xs)) xs)
        ((and (= 1 (length xs)) (list? (car xs))) (apply f (car xs)))
        (else apply f xs)))
(define (min-of . xs) (applicator min xs))
(define (max-of . xs) (applicator max xs))
(define (sum-of . xs) (applicator + xs))

(define (factors n)
  "Return all the factors of N including 1 and itself."
  (let ((i 2) (m (quotient n 2)) (r '(1)))
    (while (< i m)
      (if (zero? (remainder n i)) (set! r (cons i r)))
      (set! i (1+ i)))
    (cons n r)))

;; domain helpers
(define (no-solution? amount coins)
  "Is it impossible to make correct change with this
combination of coins?"
  (or (null? coins)
      (negative? amount)
      (and (> amount 0) (< amount (min-of coins)))))

;; external interface
(define (change amount coins)
  "Return AMOUNT using the fewest COINS (a list of coin
denominations. Returns a list of the coins required."
  ;; guard clauses for edge cases
  (cond ((zero? amount) '())
        ((no-solution? amount coins) (error 'change "no way to make correct change" amount coins))
        ((member amount coins) (list amount))
        (else (changer amount coins))))

;; main driver
(define (changer amount coins)
  "Return the best solution found for making change with
the coins provided. Best is defined as fewest coins needed."
  (letrec*
      ((original (list amount (sort coins <)))
       (amt (car original))
       (cns (list->vector (cadr original)))
       (reset (lambda ()
                (set! amt (car original))
                (set! cns (list->vector (cadr original)))))
       (simple-res '())
       (simple-fun (lambda (a c)
                     (newline)))
       )
    '()))






;; some old test code
(define (xchange amount coins)
  "Return AMOUNT using the smallest number of COINS. The
COINS list provides the denominations available. Returns
a list of coins"

  (display " ") (newline)

  ;; assorted helpers
  (define (min-coin) (apply min coins))
  (define (max-coin) (apply max coins))

  ;; quick error checks
  (if (null? coins)
      (error 'change "no coins available to make change" amount coins))
  (if (negative? amount)
      (error 'change "can not make change for a negative amount" amount coins))
  (if (and (> amount 0) (< amount (min-coin)))
      (error 'change "no coins small enough to make change" amount coins))

  (letrec*
      (
       (f-max-min (lambda (a xs) ; xs must be sorted in descending order
                    (cond ((zero? a) '())
                          ((null? xs) (error 'change "try again" amount coins)) ; need to backtrack
                          ;; pop one of the last coins off the denom list and start over
                          ;; see case of (27 '(4 5))
                          ;; should return 4 4 4 5 5 5 but
                          ;; this code errors because it's gone too deep
                          ;; keep putting a coin back into the amount until denom changes
                          ;; then put one of the new denom back
                          ;; and start over from there
                          ;; need to use a vector instead of list for backtracking
                          (else
                           (let ((c (car xs)) (cs '()))
                             (while (and (> a 0) (<= c a))
                               (set! cs (cons c cs))
                               (set! a (- a c)))
                             (append cs (f-max-min a (cdr xs))))))))
       (t-max-min (f-max-min amount (sort coins >)))
       (a-max-min (- amount (apply + t-max-min)))
       (f-factors (lambda (a xs) ('())
                          ))
       )
    (display t-max-min) (newline)
    (display a-max-min) (newline)
    t-max-min
    ))




