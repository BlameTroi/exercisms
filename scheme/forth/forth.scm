(use-modules (srfi srfi-14))

;; Simple Forthish evaluator:

;; Problem as proposed on exercism:
;;
;; - `+`, `-`, `*`, `/` (integer arithmetic)
;; - `DUP`, `DROP`, `SWAP`, `OVER` (stack manipulation)
;;
;; Your evaluator also has to support defining new words using the
;; customary syntax: `: word-name definition ;`.
;;
;; To keep things simple the only data type you need to support is
;; signed integers of at least 16 bits size.


;;
;; add remainder operator, and or not, emit, and maybe strings for
;; fun later.
;;
;; forth tokenizing rules are easy peasy ... whitespace, but to get
;; started just looking for space
;;
;; Using a dictionary almost the same way a real Forth interpreter
;; would means that built-in/core-word/primitive Scheme functions
;; must appear in source before their reference in the core words
;; dictionary.

(define (forth program)
  "Simple Forthish evaluator. Words can be defined, integer arithmetic
basic operations, and the four obvious stack manipulations are
available. All state resets between calls. PROGRAM is a list of strings
as we would see if a user was entering text at a prompt.

Returns stack contents to caller."

  (cond

   ;; allow for listlessness
   ((string? program) (forth (list program)))

   ;; basic error handling
   ((null? program) (error 'forth "you need to give me something to do" program))
   ((not (all-strings? program)) (error 'forth "program should be a list of strings" program))

   ;; standard invocation per problem spec, a list of strings
   (else
    ;; set up default environment
    (clear-stack)
    (base-dec)
    (clear-user-words)
    (set! byebye #f)
    (set! compiling #f)
    (map fparse program)
    param-stack)))


(define (all-strings? xs)
  "Are all the elements of list XS strings?"
  (letrec
      ((f (lambda (x)
            (cond
             ((null? x) #t)
             ((string? (car x)) (f (cdr x)))
             (else #f)))))
    (cond
     ((not (list? xs)) #f)
     ((null? xs) #f)
     (else (f xs)))))



(define (fparse line)
  "See the line, split the line, do each item on the line. Forth
parsing is very simple."

  (map feval (string-split line (char-set #\space #\tab #\nl)))
  param-stack)


(define (feval word)
  (let ((word-func (proc-for word core-words)))
    (cond
     ;; empty string happens when multiple delimiters hit on split
     ((string= "" word) )

     ;; if compiling, we just skip for now
     ((string= ":" word) (set! compiling #t))
     ((string= ";" word) (set! compiling #f))
     (compiling )

     ;; once the user says bye, skip until end
     ((string-ci= "bye" word) (set! byebye #t))
     (byebye )

     ;; word found in dictionaries?
     ((not (equal? 'word-not-found word-func)) (apply word-func '()))

     ;; number in supported radix? allowing for others would be nice
     ((and (= radix 16) (string-every chars-hex word)) (push (string->number word radix)))
     ((and (= radix 10) (string-every chars-dec word)) (push (string->number word radix)))
     ((and (= radix 8)  (string-every chars-oct word)) (push (string->number word radix)))
     ((and (= radix 2)  (string-every chars-bin word)) (push (string->number word radix)))

     ;; I'm sorry Dave, I can't do that
     (else (error 'feval "unknown or undefined forth word" word radix param-stack)))))


;;; param-stack.scm ;;;
;; global forth parameter stack and api


(define param-stack '())

(define (clear-stack)
  (set! param-stack '()))

(define (push n)
  (set! param-stack (cons n param-stack)))

(define (pop)
  ;; checking depth should be done elsewhere, we'll
  ;; allow a crash here
  (let ((n (car param-stack)))
    (set! param-stack (cdr param-stack)) n))

(define (deep-enough? n)
  (<= n (length param-stack)))

(define (check-stack n sym)
  (cond ((deep-enough? n) #t)
        (else (error check-stack
                     "stack underflow on op"
                     sym n (length param-stack) param-stack))))

;; core word implementation

;; radix

(define radix 10)
(define (base?) radix)
(define (base)
  (check-stack 1 'base)
  (set! radix (pop)))

(define (base-hex) (push 16) (base))
(define (base-dec) (push 10) (base))
(define (base-oct) (push 8)  (base))
(define (base-bin) (push 2)  (base))

;; character sets for numeirc evaluation

(define chars-hex (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                            #\a #\b #\c #\d #\e #\f
                            #\A #\B #\C #\D #\E #\F))

(define chars-dec (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define chars-oct (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(define chars-bin (char-set #\0 #\1))


;; stack operations

(define (dup)
  (check-stack 1 'dup)
  (let ((n (pop))) (push n) (push n)))

(define (drop)
  (check-stack 1 'drop)
  (pop))

(define (swap)
  (check-stack 2 'swap)
  (let ((n (pop)) (m (pop))) (push n) (push m)))

(define (over)
  (check-stack 2 'over)
  (push (cadr param-stack))) ;; NOTE: violates api via direct access

(define (dot-s)
  (string-join (map number->string param-stack) " "))

;; integer arithmetic
(define (op-)
  (check-stack 2 '-)
  (let* ((n (pop)) (m (pop)) (r (- m n))) (push r)))

(define (op+)
  (check-stack 2 '+)
  (let* ((n (pop)) (m (pop)) (r (+ m n))) (push r)))

(define (op*)
  (check-stack 2 '*)
  (let* ((n (pop)) (m (pop)) (r (* m n))) (push r)))

(define (op/)
  (check-stack 2 '/)
  (let* ((n (pop)) (m (pop)) (r (quotient m n))) (push r)))

(define (mod)
  (check-stack 2 'mod)
  (let* ((n (pop)) (m (pop)) (r (remainder m n))) (push r)))

(define (/mod)
  (check-stack 2 '/mod)
  (let* ((n (pop)) (m (pop)) (r (remainder m n)) (q (quotient m n))) (push q) (push r)))

;; constants
(define (c0)
  (push 0))

(define (c1)
  (push 1))

(define (c-1)
  (push -1))


;;; dictionary.scm ;;;
(define compiling #f)
(define byebye #f)

(define (proc-for word words)
  (cond ((null? words) 'word-not-found)
        ((string-ci= word (car (car words))) (cdr (car words)))
        (else (proc-for word (cdr words)))))

(define (clear-user-words) (set! user-words '()))

;; these are words that can not be redefined
(define perm-words
  '("base" "hexadecimal" "hex" "decimal" "dec" "octal" "oct"
    "binary" "bin"
    "-1" "0" "1"
    "bye" "help" "load" "save"
    ".""" "." """" "variable" "constant"
    ":" ";"))

(define core-words
  (list
   ;; constants
   (cons "-1" c-1) (cons "0" c0) (cons "1" c1)

   ;; radix related, allowing some synonyms
   (cons "base" base) (cons "base?" base?) (cons "radix" base?)
   (cons "hexadecimal" base-hex) (cons "hex" base-hex)
   (cons "decimal" base-dec) (cons "dec" base-dec)
   (cons "octal" base-oct) (cons "oct" base-oct)
   (cons "binary" base-bin) (cons "bin" base-bin)

   ;; stack manipulation
   (cons "dup" dup) (cons "drop" drop) (cons "swap" swap)
   (cons "over" over)

   ;; primitive arithmetic
   (cons "+" op+) (cons "-" op-) (cons "/" op/)
   (cons "*" op*) (cons "mod" mod) (cons "/mod" /mod)
   ))

(define user-words '())

;;

;; vocab definition from brodie's forth book
;; +	( n1 n2 — sum )	Adds
;; –	( n1 n2 — diff )	Subtracts (n1-n2)
;; *	( n1 n2 — prod )	Multiplies
;; /	( n1 n2 — quot )	Divides (n1/n2)
;; /MOD	( n1 n2 — rem quot )	Divides; returns remainder and quotient
;; MOD	( n1 n2 — rem )	Divides; returns remainder only
;; SWAP	( n1 n2 — n2 n1 )	Reverses the top two stack items
;; DUP	( n — n n )	Duplicates the top stack item
;; OVER	( n1 n2 — n1 n2 n1 )	Copies second item to top
;; ROT	( n1 n2 n3 — n2 n3 n1 )	Rotates third item to top
;; DROP	( n — )	Discards the top stack item
;; 2SWAP	( d1 d2 — d2 d1 )	Reverses the top two pairs of numbers
;; 2DUP	( d — d d )	Duplicates the top pair of numbers
;; 2OVER	( d1 d2 — d1 d2 d1 )	Duplicates the second pair of numbers
;; 2DROP	( d1 d2 — d1 )	Discards the top pair of numbers
