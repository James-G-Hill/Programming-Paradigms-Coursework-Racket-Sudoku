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

;;;
(define (unReplace list)
  (if (> (length list) 1) 0 (list-ref (flatten list) 0)))

;;; Transforms the matrix into a list of lists of sets.
(define (transform matrix)
  (map (lambda (x) (map setReplace x)) matrix))

;;;
(define (untransform matrix)
  (map (lambda (x) (map unReplace x)) matrix))

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
;;;  THE SOLVER
;;;

;;; The solve function.
(define (solve matrix)
  (if (< (countNonSingletons (transform matrix)) 50)
      matrix
      (solve (untransform (loopMatrix (transform matrix) 1 1)))))

;;;
;;;  MAIN ALGORITHM
;;;

;;; Loop through the matrix applying the algorithm.
(define (loopMatrix matrix r c)
  (if (< c 8)
      (loopMatrix (applyAlgorithm matrix r c) r (+ c 1))
      (if (< r 8)
          (loopMatrix (applyAlgorithm matrix r 1) (+ r 1) 1) matrix)))

;;; Apply the algorithm.
(define (applyAlgorithm matrix row col)
  (let ([digit (list-ref (list-ref matrix (- row 1)) (- col 1))])
    (if (checkSingleton digit)
        (removeNumberSquare
         (removeNumberColumn
          (removeNumberRow
           matrix (- row 1) (first digit))
          (- col 1) (first digit))
         (- row 1) (- col 1) (first digit))
        matrix)))

;;; Remove digit from row.
(define (removeNumberRow matrix row digit)
  (let ([x (list-ref matrix row)])
    (list-set matrix row (map (lambda (y) (removeNumber y digit)) x))))

;;; Create list of non-duplicates from surrounding cells.
(define (combineNonDupeLists matrix row col)
  (sort (remove-duplicates
   (flatten
    (list
     (returnUniqueListRow matrix row col)
     (returnUniqueListCol matrix row col)
     (returnUniqueListSquare matrix row col))))
        <))

;;; Remove duplicates from a row.
(define (returnUniqueListRow matrix row col)
  (let ([x (list-ref matrix row)])
    (remove-duplicates (flatten (append (take x col) (drop x (+ col 1)))))))

;;; Remove duplicates from a column.
(define (returnUniqueListCol matrix row col)
  (let ([C (map (lambda (x) (list-ref x col)) matrix)])
    (remove-duplicates (flatten (append (take C row) (drop C (+ row 1)))))))

;;; Remove duplicates from a square.
(define (returnUniqueListSquare matrix row col)
  (remove-duplicates (flatten (returnNumberSquareSet matrix row col))))

;;; Remove digit from set.
;(define (removeNumberRowSet matrix row column digit)
;  (let ([x (list-ref matrix row)])
;    (list-set matrix row (if (> (foldl + 0 (map (lambda (y) (if (member digit y) 1 0)) x)) 1) x (list-set x column (list digit))))))

;;; Remove the digit from column.
(define (removeNumberColumn matrix column digit)
  (map (lambda (x) (list-set x column (removeNumber (list-ref x column) digit))) matrix))

;;; Remove the digit from column set.
;(define (removeNumberColumnSet matrix row column digit)
;  (let ([R (list-ref matrix row)])
;    (if (> (foldl + 0 (map (lambda (x) (if (member digit (list-ref x column)) 1 0)) matrix)) 1)
;        matrix
;        (list-set matrix row (list-set R column (list digit))))))

;;; Remove the digit from the square.
(define (removeNumberSquare matrix row col digit)
  (list
   (if (not (outsideSquare 0 row)) (updateRow (first matrix) col digit) (first matrix))
   (if (not (outsideSquare 1 row)) (updateRow (second matrix) col digit) (second matrix))
   (if (not (outsideSquare 2 row)) (updateRow (third matrix) col digit) (third matrix))
   (if (not (outsideSquare 3 row)) (updateRow (fourth matrix) col digit) (fourth matrix))
   (if (not (outsideSquare 4 row)) (updateRow (fifth matrix) col digit) (fifth matrix))
   (if (not (outsideSquare 5 row)) (updateRow (sixth matrix) col digit) (sixth matrix))
   (if (not (outsideSquare 6 row)) (updateRow (seventh matrix) col digit) (seventh matrix))
   (if (not (outsideSquare 7 row)) (updateRow (eighth matrix) col digit) (eighth matrix))
   (if (not (outsideSquare 8 row)) (updateRow (ninth matrix) col digit) (ninth matrix)))
  )

;;; Remove the digit from the square.
(define (updateRow matrix col digit)
  (list
   (if (not (outsideSquare 0 col)) (removeNumber (first matrix) digit) (first matrix))
   (if (not (outsideSquare 1 col)) (removeNumber (second matrix) digit) (second matrix))
   (if (not (outsideSquare 2 col)) (removeNumber (third matrix) digit) (third matrix))
   (if (not (outsideSquare 3 col)) (removeNumber (fourth matrix) digit) (fourth matrix))
   (if (not (outsideSquare 4 col)) (removeNumber (fifth matrix) digit) (fifth matrix))
   (if (not (outsideSquare 5 col)) (removeNumber (sixth matrix) digit) (sixth matrix))
   (if (not (outsideSquare 6 col)) (removeNumber (seventh matrix) digit) (seventh matrix))
   (if (not (outsideSquare 7 col)) (removeNumber (eighth matrix) digit) (eighth matrix))
   (if (not (outsideSquare 8 col)) (removeNumber (ninth matrix) digit) (ninth matrix)))
  )

;;; Return a list of numbers from an internal square.
(define (returnNumberSquareSet matrix row col)
  (list
   (if (not (outsideSquare 0 row)) (returnRowSquare (first matrix) (= 0 row) col) '())
   (if (not (outsideSquare 1 row)) (returnRowSquare (second matrix) (= 1 row) col) '())
   (if (not (outsideSquare 2 row)) (returnRowSquare (third matrix) (= 2 row) col) '())
   (if (not (outsideSquare 3 row)) (returnRowSquare (fourth matrix) (= 3 row) col) '())
   (if (not (outsideSquare 4 row)) (returnRowSquare (fifth matrix) (= 4 row) col) '())
   (if (not (outsideSquare 5 row)) (returnRowSquare (sixth matrix) (= 5 row) col) '())
   (if (not (outsideSquare 6 row)) (returnRowSquare (seventh matrix) (= 6 row) col) '())
   (if (not (outsideSquare 7 row)) (returnRowSquare (eighth matrix) (= 7 row) col) '())
   (if (not (outsideSquare 8 row)) (returnRowSquare (ninth matrix) (= 8 row) col) '())))

;;; Return a list of numbers from a row.
(define (returnRowSquare matrix row col)
  (list
   (if (not (outsideSquare 0 col)) (if (and row (= col 0)) '() (first matrix)) '())
   (if (not (outsideSquare 1 col)) (if (and row (= col 1)) '() (second matrix))'())
   (if (not (outsideSquare 2 col)) (if (and row (= col 2)) '() (third matrix)) '())
   (if (not (outsideSquare 3 col)) (if (and row (= col 3)) '() (fourth matrix)) '())
   (if (not (outsideSquare 4 col)) (if (and row (= col 4)) '() (fifth matrix)) '())
   (if (not (outsideSquare 5 col)) (if (and row (= col 5)) '() (sixth matrix)) '())
   (if (not (outsideSquare 6 col)) (if (and row (= col 6)) '() (seventh matrix)) '())
   (if (not (outsideSquare 7 col)) (if (and row (= col 7)) '() (eighth matrix)) '())
   (if (not (outsideSquare 8 col)) (if (and row (= col 8)) '() (ninth matrix)) '())))

;;; Check rows and columns still within square.
(define (outsideSquare original current)
  (or
   (< current (reduceSquareRootNumber original))
   (> current (+ (reduceSquareRootNumber original) 2))))

;;; Reduce to square root number.
(define (reduceSquareRootNumber digit)
  (cond
    [(< digit 3) 1]
    [(< digit 6) 4]
    [(< digit 9) 7]))

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
         combineNonDupeLists
         countNonSingletons
         outsideSquare
         reduceSquareRootNumber
         removeNumber
         removeNumberColumn
;         removeNumberColumnSet
         removeNumberRow
;         removeNumberRowSet
         removeNumberSquare
;         removeNumberSquareSet
         returnUniqueListCol
         returnUniqueListRow
         returnUniqueListSquare
         setReplace
         transform
         untransform)
