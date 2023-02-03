(import (rnrs))

(define (anagram target words)
  "Return all strings in 'words' that are anagrams of 'target'.
Check are case insensitive, a string can not be an anagram of
itself, and all characters are significant."
  (if (or (= 0 (string-length target)) (= 0 (length words)))
      '()
      (let*
          ([tl (string-length target)]
           [string->sorted-chars (lambda (s) (list->string (list-sort char-ci<? (string->list s))))]
           [tc (string->sorted-chars target)])
        (filter
         (lambda (w)
           (cond
            ((not (= tl (string-length w))) #f)
            ((string-ci=? target w) #f)
            ((not (string-ci=? tc (string->sorted-chars w))) #f)
            (#t #t)))
         words))))
