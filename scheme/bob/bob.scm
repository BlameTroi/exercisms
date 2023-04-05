(import (rnrs))

;; another over engineered solution :)

(define (message-scrub s)
  "Replace whitespace in message with blanks."
  (list->string
   (map
    (lambda (c) (cond ((or (char=? c #\tab)
                           (char=? c #\newline)
                           (char=? c #\return)
                           (char=? c #\page)) #\space)
                      (else c)))
    (string->list s))))

(define (message-deblank s)
  "Remove leading and trailing blanks, reduce all blanks to
a single blank."
  (string-join
   (filter
    (lambda (x) (string<> "" x))
    (string-split s #\space))
   " "))

(define (message-all-caps? s)
  "Test letters for case."
  (letrec*
      ((caps-seen #f)
       (lower-seen #f)
       (done #f)
       (f (lambda (xs)
            (cond ((null? xs) (set! done #t))
                  ((and caps-seen
                        (and (char>=? (car xs) #\a)
                             (char<=? (car xs) #\z))) (set! lower-seen #t))
                  ((and (char>=? (car xs) #\A)
                        (char<=? (car xs) #\Z)) (set! caps-seen #t) )
                  (else '()))
            (if done '() (f (cdr xs))))))
    (f (string->list s))
    (and caps-seen (not lower-seen))))

(define message-question       1)
(define message-yelling        2)
(define message-yell-question  3)
(define message-silence        4)
(define message-what           0)

(define (message-category message)
  "Assign category based on rules for capital letters and
punctuation."
  (let ((res 0) (s (message-deblank (message-scrub message))))
    ;; tis query?
    (set! res (+ res
                 (if (and (string-rindex s #\?)
                          (= (string-length s) (1+ (string-rindex s #\?))))
                     message-question
                     0)))
    (set! res (+ res
                 (if (message-all-caps? s)
                     message-yelling
                     0)))
    (set! res (+ res
                 (if (= 0 (string-length s))
                     message-silence
                     0)))
    res))

(define (response-for message)
  "Bob responds predictably."
  (let ((cat (message-category message)))
    (cond ((= cat message-silence) "Fine. Be that way!")
          ((= cat message-yelling) "Whoa, chill out!")
          ((= cat message-yell-question) "Calm down, I know what I'm doing!")
          ((= cat message-question) "Sure.")
          (else "Whatever."))))
