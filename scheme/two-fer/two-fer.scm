(import (rnrs))

(define (two-fer . maybe-name)
  (string-append "One for " (if (null? maybe-name) "you" (car maybe-name)) ", one for me."))
