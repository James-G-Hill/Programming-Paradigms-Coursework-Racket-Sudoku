#lang racket

(require rackunit
         "Sudoku.rkt")

;;;
;;;  TESTING VARIABLES
;;;

;;; A row for testing.
(define row
  (list
   (list 1 2 3 4 5 6 7 8 9)
   (list 2)
   (list 5)
   (list 1 2 3 4 5 6 7 8 9)
   (list 1 2 3 4 5 6 7 8 9)
   (list 1)
   (list 1 2 3 4 5 6 7 8 9)
   (list 1 2 3 4 5 6 7 8 9)
   (list 1 2 3 4 5 6 7 8 9)))


;;;
;;;  TRANSFORM TESTING
;;;

;;; Test 'setReplace'
(check-equal? (setReplace 0) (list 1 2 3 4 5 6 7 8 9) "Replace Zero with a List (1-9)")
(check-equal? (setReplace 1) (list 1) "Replace 1 with a List (1)")
(check-equal? (setReplace 9) (list 9) "Replace 1 with a List (9)")