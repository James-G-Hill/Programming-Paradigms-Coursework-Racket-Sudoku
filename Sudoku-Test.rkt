#lang racket

(require rackunit
         "Sudoku.rkt")

;;;
;;;  TESTING VARIABLES
;;;

;;; The soduko matrix provided with the data brief.
(define puzzle
  (list
   (list 0 2 5 0 0 1 0 0 0)
   (list 1 0 4 2 5 0 0 0 0)
   (list 0 0 6 0 0 4 2 1 0)
   (list 0 5 0 0 0 0 3 2 0)
   (list 6 0 0 0 2 0 0 0 9)
   (list 0 8 7 0 0 0 0 6 0)
   (list 0 9 1 5 0 0 6 0 0)
   (list 0 0 0 0 7 8 1 0 3)
   (list 0 0 0 6 0 0 5 9 0)))

;;; A transformed version of the above matrix.
(define transformedPuzzle
  '(((1 2 3 4 5 6 7 8 9) (2) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1) (1 2 3 4 5 6 7 8 9) (4) (2) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (4) (2) (1) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (3) (2) (1 2 3 4 5 6 7 8 9))
  ((6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (2) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (9))
  ((1 2 3 4 5 6 7 8 9) (8) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (9) (1) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (7) (8) (1) (1 2 3 4 5 6 7 8 9) (3))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (5) (9) (1 2 3 4 5 6 7 8 9))))

;;; A row for testing.
(define row
  (list 0 2 5 0 0 1 0 0 0))

;;; A 'transformed' row for testing.
(define transformedRow
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

;;; Test 'setReplace'.
(check-equal? (setReplace 0) (list 1 2 3 4 5 6 7 8 9) "Replace Zero with a List (1-9)")
(check-equal? (setReplace 1) (list 1) "Replace 1 with a List (1)")
(check-equal? (setReplace 9) (list 9) "Replace 1 with a List (9)")

;;; Test 'transform'.
(check-equal? (transform puzzle) transformedPuzzle "Transform a whole puzzle.")