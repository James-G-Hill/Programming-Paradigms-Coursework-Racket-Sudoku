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