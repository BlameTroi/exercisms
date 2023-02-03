Chez Scheme Version 9.5
Copyright 1984-2017 Cisco Systems, Inc.

> (define silent-listen (lambda () '("Listen" "Silent" "LISTEN"))
    )
> (silent-listen)
("Listen" "Silent" "LISTEN")
> (map (lambda (x) (string->list x)) (silent-listen))
((#\L #\i #\s #\t #\e #\n)
  (#\S #\i #\l #\e #\n #\t)
  (#\L #\I #\S #\T #\E #\N))
> (string-ci=? "listen" "LISTEN")
#t
> (sort '("a" "b" "c" "e" "d"))

Exception: incorrect argument count in call (sort (quote ("a" "b" "c" "e" "d")))
Type (debug) to enter the debugger.
> (sort < '("a" "b" "c" "e" "d"))

Exception in <: "d" is not a real number
Type (debug) to enter the debugger.
> (sort string< '("a" "c" "b"))

Exception: variable string< is not bound
Type (debug) to enter the debugger.
> (sort string<? '("a" "c" "b"))
("a" "b" "c")
> (list-sort string<? '("a" "C" "A" "b"))
("A" "C" "a" "b")
> (list-sort string<? '("strings" "and" "more" "strings"))
("and" "more" "strings" "strings")
> (define length-check
    (lambda (e)
      (= (string-length w) (string-length e))))
> (define w "fred")
> (length-check "wilma")
#f
> (length-check "troy")
#t
> (map length-check '("fred" "wilma" "barney" "george" "tina" "troy"))
(#t #f #f #f #t #t)
> (filter length-check '("fred" "wilma" "barney" "george" "tina" "troy"))
("fred" "tina" "troy")
> (define w "wilma")
> (filter length-check '("fred" "wilma" "barney" "george" "tina" "troy"))
("wilma")
> (define w "xxxxxx")
> (filter length-check '("fred" "wilma" "barney" "george" "tina" "troy"))
("barney" "george")
> (car ("troy" . "orty"))

Exception: invalid syntax ("troy" . "orty")
Type (debug) to enter the debugger.
> ("troy" . "orty")

Exception: invalid syntax ("troy" . "orty")
Type (debug) to enter the debugger.
> '("troy" . "orty")
("troy" . "orty")
> (car '("troy" . "orty")
"troy"
> (cdr '("troy" . "orty"))
"orty"
> (define filter-value "allergy")
(define filter-length-value (string-length filter-value))
(define candidates '("gallery" "ballerina" "regally" "clergy" "clerical" "asdf" "largely" "leading"))
(define filter-length (x)
  (cond
   ((string=? filter-value x) #f)
   ((= filter-length-value (string-length x) #t))
   (#t #f)))
(filter filter-length candidates)

> > >
Exception: invalid syntax (define filter-length (x) (cond ((...) #f) ((...)) (#t #f)))
Type (debug) to enter the debugger.
>
Exception: variable filter-length is not bound
Type (debug) to enter the debugger.
> #f
> (define filter-length (lambda (x)
  (cond
   ((string=? filter-value x) #f)
   ((= filter-length-value (string-length x) #t))
   (#t #f))))
> (filter filter-length candidates)

Exception in =: #t is not a number
Type (debug) to enter the debugger.
> filter-length-value
7
> (define filter-length (lambda (x)
  (cond
   ((string-ci=? filter-value x) #f)
   ((= filter-length-value (string-length x)) #t)
   (#t #f))))
(filter filter-length candidates)

> ("gallery" "regally" "largely" "leading")
> #f
> (filter filter-length candidates)
("gallery" "regally" "largely" "leading")
> candidates
("gallery" "ballerina" "regally" "clergy" "clerical" "asdf"
  "largely" "leading")
> (define filter-length (lambda (x)
  (cond
   ((string-ci=? filter-value x) #f)
   ((= filter-length-value (string-length x)) #t)
   (#t #f))))
(filter filter-length candidates)

> ("gallery" "regally" "largely" "leading")
> #f
> (filter filter-length candidates)
("gallery" "regally" "largely" "leading")
> (define candidates (list "allergy" "AllERGY" candidates))

Exception: variable set is not bound
Type (debug) to enter the debugger.
> (define candidates (list "alergy" "Allergy" ALLergy" "edgy" "gallery" "ballerina" "regally" "clergy" "clerical" "staff"))
> candidates
("allergy"
  "AllERGY"
  ("gallery" "ballerina" "regally" "clergy" "clerical" "asdf"
    "largely" "leading"))
> (define candidates (list "alergy" "Allergy" "ALLergy" "edgy" "gallery" "ballerina" "regally" "clergy" "clerical" "staff"))
  <menu-bar> <signals> <break>(define candidates (list "alergy" "Allergy" "ALLergy" "edgy" "gallery" "ballerina" "regally" "clergy" "clerical" "staff"))
  <menu-bar> <signals> <break>

  <menu-bar> <signals> <kill>
It's been nice interacting with you!
Press C-c C-z to bring me back.
Chez Scheme Version 9.5
Copyright 1984-2017 Cisco Systems, Inc.

> (define filter-value "allergy")
(define filter-length-value (string-length filter-value))
(define candidates '("gallery" "ballerina" "regally" "clergy" "clerical" "asdf" "largely" "leading" "allergy" "allergies" "yellreg" "ALLERGY" "staff" "AlLeRgY"))
(define filter-length (lambda (x)
  (cond
   ((string-ci=? filter-value x) #f)
   ((not (= filter-length-value (string-length x))) #f)
   (#t #t))))
(filter filter-length candidates)

> > > > ("gallery" "regally" "largely" "leading" "yellreg")
> #f
> (string-length "fred")
4
> 
(define (anagram target words)
  (if (or (= 0 (string-length target)) (= 0 (length words)))
      '()
      (let*
          ([tl (string-length target)]
           [string->sorted-chars (lambda (s) (list->string (list-sort char-ci<? (string->list s))))])
        [tc (string->sorted-chars target)]
        (filter
         (lambda (w)
           (cond
            ((not (= tl (string-length w))) #f)
            ((string-ci=? target w) #f)
            ((not (string-ci=? tc (string->sorted-chars w))) #f)
            (#t #t)))))))

> #f
> candidates
("gallery" "ballerina" "regally" "clergy" "clerical" "asdf" "largely"
  "leading" "allergy" "allergies" "yellreg" "ALLERGY" "staff"
  "AlLeRgY")
> (anagram "allergy" candidates)

Exception: variable tc is not bound
Type (debug) to enter the debugger.
> 
(define (anagram target words)
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
            (#t #t)))))))

> #f
> (anagram "allergy" candidates)

Exception: incorrect argument count in call (filter (lambda (w) (if (...) #f (...))))
Type (debug) to enter the debugger.
> (define (anagram target words)
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

> #f
> (anagram "allergy" candidates)
("gallery" "regally" "largely")
> (anagram "allergy" candidates)
It's been nice interacting with you!
Press C-c C-z to bring me back.
