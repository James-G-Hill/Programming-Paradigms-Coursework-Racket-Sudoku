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

;;; A transformed version of the above matrix with 4 removed from column 2.
(define transformedPuzzle4Removed
  '(((1 2 3 4 5 6 7 8 9) (2) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1) (1 2 3 5 6 7 8 9) (4) (2) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (4) (2) (1) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (3) (2) (1 2 3 4 5 6 7 8 9))
  ((6) (1 2 3 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (2) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (9))
  ((1 2 3 4 5 6 7 8 9) (8) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (9) (1) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (7) (8) (1) (1 2 3 4 5 6 7 8 9) (3))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (5) (9) (1 2 3 4 5 6 7 8 9))))

;;; A transformed version of the above matrix.
(define transformedPuzzleTopLeftSquare4Removed
  '(((1 2 3 5 6 7 8 9) (2) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1) (1 2 3 5 6 7 8 9) (4) (2) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 5 6 7 8 9) (1 2 3 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (4) (2) (1) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (3) (2) (1 2 3 4 5 6 7 8 9))
  ((6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (2) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (9))
  ((1 2 3 4 5 6 7 8 9) (8) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (9) (1) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (7) (8) (1) (1 2 3 4 5 6 7 8 9) (3))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (5) (9) (1 2 3 4 5 6 7 8 9))))

;;; A singleton for testing.
(define singleton
  (list 1))

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

;;; A 'transformed' row for testing.
(define transformedRow5Removed
  (list
   (list 1 2 3 4 6 7 8 9)
   (list 2)
   (list 5)
   (list 1 2 3 4 6 7 8 9)
   (list 1 2 3 4 6 7 8 9)
   (list 1)
   (list 1 2 3 4 6 7 8 9)
   (list 1 2 3 4 6 7 8 9)
   (list 1 2 3 4 6 7 8 9)))


;;;
;;;  TRANSFORM TESTING
;;;

;;; Test 'setReplace'.
(check-equal? (setReplace 0) (list 1 2 3 4 5 6 7 8 9) "Replace Zero with a List (1-9)")
(check-equal? (setReplace 1) (list 1) "Replace 1 with a List (1)")
(check-equal? (setReplace 9) (list 9) "Replace 1 with a List (9)")

;;; Test 'transform'.
(check-equal? (transform puzzle) transformedPuzzle "Transform a whole puzzle.")

;;;
;;;  COUNTING TESTING
;;;

;;; Test 'checkSingleton'.
(check-equal? (checkSingleton singleton) #t "Checks singleton is true")
(check-equal? (checkSingleton row) #f "Checks singleton is false")

;;; Test 'countNonSingletons'.
(check-equal? (countNonSingletons transformedPuzzle) 50 "Count singletons correctly")

;;;
;;;  MAIN ALGORITHM TESTING
;;;

;;; Test 'removeNumber'.
(check-equal? (removeNumber row 5) (list 0 2 0 0 1 0 0 0) "Remove a digit from a set")
(check-equal? (removeNumber row 9) (list 0 2 5 0 0 1 0 0 0) "Attempt to remove a digit from a set")

;;; Test 'removeNumberRow'.
(check-equal? (removeNumberRow transformedRow 5) transformedRow5Removed "Remove a digit from all sets in a row")
(check-equal? (removeNumberRow transformedRow5Removed 5) transformedRow5Removed "Attempt to remove a digit from all sets in a row")

;;; Test 'removeNumberColumn'.
(check-equal? (removeNumberColumn transformedPuzzle 2 4) transformedPuzzle4Removed "Removed digit from column")
(check-equal? (removeNumberColumn transformedPuzzle4Removed 2 4) transformedPuzzle4Removed "Attempted to remove already removed digit from column")

;;; Test 'removeNumberSquare'.
;(check-equal? (removeNumberSquare transformedPuzzle 2 3 4) transformedPuzzleTopLeftSquare4Removed "Removed digit from square")
;(check-equal? (removeNumberSquare transformedPuzzleTopLeftSquare4Removed 2 3 4) transformedPuzzleTopLeftSquare4Removed "Attempted to remove already removed digit from square")

;;; Test 'withinSquare'.
(check-equal? (outsideSquare 1 1 1 1) #f "Within square")
(check-equal? (outsideSquare 1 4 1 1) #t "Testing row")
(check-equal? (outsideSquare 1 1 1 4) #t "Testing column")
(check-equal? (outsideSquare 4 4 4 4) #f "Within square")
(check-equal? (outsideSquare 4 7 4 4) #t "Testing row")
(check-equal? (outsideSquare 4 4 4 7) #t "Testing column")
(check-equal? (outsideSquare 7 7 7 7) #f "Within square")
(check-equal? (outsideSquare 7 10 7 7) #t "Testing row")
(check-equal? (outsideSquare 7 7 7 10) #t "Testing column")

;;; Test 'reduceSquareRootNumber'.
(check-equal? (reduceSquareRootNumber 1) 1 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 2) 1 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 3) 1 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 4) 4 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 5) 4 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 6) 4 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 7) 7 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 8) 7 "Reduce digit to square root")
(check-equal? (reduceSquareRootNumber 9) 7 "Reduce digit to square root")
