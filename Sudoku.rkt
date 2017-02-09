#lang racket

;;;
;;;  VARIABLES
;;;

;;; The intitial soduko matrix provided with the data brief.
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

;;;
;;;  TRANSFORM FUNCTION
;;;

;;; Replaces digit 0 with set (1-9) or digit 1-9 with the digit.
(define (setReplace digit)
  (if (= 0 digit)
      (list 1 2 3 4 5 6 7 8 9)
      (list digit)))

;;; Transforms the matrix into a list of lists of sets.
(define (transform matrix)
  (map (lambda (x) (map setReplace x)) matrix))


;;;
;;;  COUNTING FUNCTION
;;;

;;; Count at the matrix level.
(define (countNonSingletons matrix)
  (foldl + 0 (map (lambda (x) (if (eq? (checkSingleton x) #f) 1 0)) (apply append matrix))))

;;; Check is singleton or not.
(define (checkSingleton set)
  (if (> (length set) 1) #f #t))


;;;
;;;  MAIN ALGORITHM
;;;

;;; Remove digit from row.
(define (removeNumberRow row digit)
  (map (lambda (x) (removeNumber x digit)) row))

;;; Remove the digit from column.
(define (removeNumberColumn matrix column digit)
  (map (lambda (x) (columnFilter x column digit)) matrix))

;;; Find the column in a row and remove the digit.
(define (columnFilter row column digit)
    (list-set row (- column 1) (removeNumber (list-ref row (- column 1)) digit)))

;;; Remove the digit from the square.
(define (removeNumberSquare matrix orRow row orCol column digit)
  (if (outsideSquare row column)
      matrix
      (list-set (removeNumberSquare (removeNumberSquare matrix row (+ 1 column) digit) (+ 1 row) column digit) (floor (/ row 3))
            (columnFilter (list-ref (removeNumberSquare (removeNumberSquare matrix row (+ 1 column) digit) (+ 1 row) column digit) (floor (/ row 3))) (ceiling (/ column 3)) digit))))

;;; Check rows and columns still within square.
(define (outsideSquare orRow row orCol column)
  (or
   (> row (+ (reduceSquareRootNumber orRow) 2))
   (> column (+ (reduceSquareRootNumber orCol) 2))))

;;; Reduce to square root number.
(define (reduceSquareRootNumber digit)
  (cond
    [(< digit 4) 1]
    [(< digit 7) 4]
    [(< digit 10) 7]))

;;; Remove number from set.
(define (removeNumber set digit)
  (if (eq? #f (checkSingleton set))
      (filter (lambda (x)  (not (= x digit))) set)
      set))


;;;
;;;  PROVIDER FUNCTION
;;;

;;; Provides all functions for testing.
(provide checkSingleton
         countNonSingletons
         outsideSquare
         reduceSquareRootNumber
         removeNumber
         removeNumberColumn
         removeNumberRow
         removeNumberSquare
         setReplace
         transform)
