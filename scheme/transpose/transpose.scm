(import (rnrs))

;; transpose a matrix, rows become columns and columns become rows
;; if input rows are of differing lengths, pad to the LEFT with
;; spaces. never pad to the right.
;; a row of all spaces is allowed.
;;
;; input test data is numeric, but since a list can be heterogeneous
;; resist the urge to use zero for pads.
;;
;; not that padding as above is ON OUTPUT.
;;
;; even          odd1          odd2
;; ABC           ABC           AB
;; DEF           DE            DEF
;;
;; AD            AD            AD
;; BE            BE            BE
;; CF            C              F

;; the incoming matrix is a list of lists where each sublist is a row.

;; and it appears that characters have been converted to their ascii
;; equivalents, making the testing a bit less obvious.

;; AND by my review so far, every test case is regular (each row same
;; length). Weird.

;; i've extracted a few tests and will work from within this file.

(define transpose) ;; forward
(define test-success) ;; forward

(define sometests
  (list
   (lambda ()
     (test-success "empty string" equal? transpose '(()) '()))
   (lambda ()
     (test-success "two characters in a row" equal? transpose
                   '(((65 49))) '((65) (49))))
   (lambda ()
     (test-success "two characters in a column" equal? transpose
                   '(((65) (49))) '((65 49))))
   (lambda ()
     (test-success "simple" equal? transpose
                   '(((65 66 67) (49 50 51))) '((65 49) (66 50) (67 51))))))

(define (test-success description
                      success-predicate
                      procedure
                      input
                      output)
  (let*
      ([received (apply procedure input)])
    (cond
     ((success-predicate output received)
      (begin
        (format #t "~%~a passed!~%" description)
        #t))
     (#t ;; else actually
      (begin
        (format #t "~%~a failed!~%   input: ~a~%expected: ~a~%received: ~a~%" description input output received)
        #f)))))

;; iterate over tests
(define (runner tests) (map (lambda (t) (t)) tests))


;; what does our matrix look like?
(define (dimensions matrix) ;; returns rows, columns, and 'jagged or 'regular
  (let*
      ([edges 'regular]
       [rows (length matrix)]
       [cols (if (= 0 rows)
                 0
                 (length (car matrix)))]) ;; cols is wrong if jagged, but we do not check yet
    (list rows cols edges)))


;; transpose the matrix
(define (transpose matrix)
  (let*
      ([shape (dimensions matrix)]
       [rows (car shape)]
       [cols (car (cdr shape))]
       [jagged (eq? 'jagged (cdr (cdr shape)))]) ;; jagged not checked yet
    (

     )))

;; considering flattening into a single list ... recursively drill down
;; creating a new list, then process the list somehow to create the new
;; matrix. in c or basic this would be done a long time ago ... new
;; language and structures ... new headaches :)
