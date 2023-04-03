(import (rnrs))


(define (nucleotide-count dna)
  "Returns an alist of the nucleotide counts in the string DNASEQ.
Returns error if a nucleotide code other than A C T or G is found."
  (let ((counts (list (cons #\A  0) (cons #\C  0) (cons #\G  0) (cons #\T  0)))
        (fragmented (string->list dna))
        (x #\nul) (v 0))
    (while (not (null? fragmented))
      (set! x (car fragmented)) (set! fragmented (cdr fragmented))
      (set! v (assoc-ref counts x))
      (if (number? v)
          (assoc-set! counts x (1+ v))
          (error 'nucleotide-count (string-append "DNA sequence has an invalid nucleotide:" (string x)))))
    counts))
