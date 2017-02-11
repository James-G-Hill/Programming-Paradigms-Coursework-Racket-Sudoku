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

;;; Remove duplicates from a row.
(define (returnUniqueListRow matrix row col)
  (let ([x (list-ref matrix row)])
    (remove-duplicates (flatten (append (take x col) (drop x (+ col 1)))))))

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
   (if (not (outsideSquare 1 row)) (updateRow (first matrix) col digit) (first matrix))
   (if (not (outsideSquare 2 row)) (updateRow (second matrix) col digit) (second matrix))
   (if (not (outsideSquare 3 row)) (updateRow (third matrix) col digit) (third matrix))
   (if (not (outsideSquare 4 row)) (updateRow (fourth matrix) col digit) (fourth matrix))
   (if (not (outsideSquare 5 row)) (updateRow (fifth matrix) col digit) (fifth matrix))
   (if (not (outsideSquare 6 row)) (updateRow (sixth matrix) col digit) (sixth matrix))
   (if (not (outsideSquare 7 row)) (updateRow (seventh matrix) col digit) (seventh matrix))
   (if (not (outsideSquare 8 row)) (updateRow (eighth matrix) col digit) (eighth matrix))
   (if (not (outsideSquare 9 row)) (updateRow (ninth matrix) col digit) (ninth matrix)))
  )

;;; Remove the digit from the square.
(define (updateRow matrix col digit)
  (list
   (if (not (outsideSquare 1 col)) (removeNumber (first matrix) digit) (first matrix))
   (if (not (outsideSquare 2 col)) (removeNumber (second matrix) digit) (second matrix))
   (if (not (outsideSquare 3 col)) (removeNumber (third matrix) digit) (third matrix))
   (if (not (outsideSquare 4 col)) (removeNumber (fourth matrix) digit) (fourth matrix))
   (if (not (outsideSquare 5 col)) (removeNumber (fifth matrix) digit) (fifth matrix))
   (if (not (outsideSquare 6 col)) (removeNumber (sixth matrix) digit) (sixth matrix))
   (if (not (outsideSquare 7 col)) (removeNumber (seventh matrix) digit) (seventh matrix))
   (if (not (outsideSquare 8 col)) (removeNumber (eighth matrix) digit) (eighth matrix))
   (if (not (outsideSquare 9 col)) (removeNumber (ninth matrix) digit) (ninth matrix)))
  )

;;; Remove the digit from the square.
;(define (removeNumberSquareSet matrix row col digit)
;  (if (>
;       (apply +
;              (list
;               (if (not (outsideSquare 1 row)) (updateRowSet (first matrix) (outsideSquare 1 row) col digit) 0)
;               (if (not (outsideSquare 2 row)) (updateRowSet (second matrix) (outsideSquare 2 row) col digit) 0)
;               (if (not (outsideSquare 3 row)) (updateRowSet (third matrix) (outsideSquare 3 row) col digit) 0)
;               (if (not (outsideSquare 4 row)) (updateRowSet (fourth matrix) (outsideSquare 4 row) col digit) 0)
;               (if (not (outsideSquare 5 row)) (updateRowSet (fifth matrix) (outsideSquare 5 row) col digit) 0)
;               (if (not (outsideSquare 6 row)) (updateRowSet (sixth matrix) (outsideSquare 6 row) col digit) 0)
;               (if (not (outsideSquare 7 row)) (updateRowSet (seventh matrix) (outsideSquare 7 row) col digit) 0)
;               (if (not (outsideSquare 8 row)) (updateRowSet (eighth matrix) (outsideSquare 8 row) col digit) 0)
;               (if (not (outsideSquare 9 row)) (updateRowSet (ninth matrix) (outsideSquare 9 row) col digit) 0))
;              )
;       1) matrix (list-set matrix row (list-set (list-ref matrix row) col (list digit)))))

;;; Remove the digit from the square.
;(define (updateRowSet matrix row col digit)
;  (apply +
;         (list
;          (if (and row (not (outsideSquare 1 col))) (if (member digit (first matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 2 col))) (if (member digit (second matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 3 col))) (if (member digit (third matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 4 col))) (if (member digit (fourth matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 5 col))) (if (member digit (fifth matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 6 col))) (if (member digit (sixth matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 7 col))) (if (member digit (seventh matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 8 col))) (if (member digit (eighth matrix)) 1 0) 0)
;          (if (and row (not (outsideSquare 9 col))) (if (member digit (ninth matrix)) 1 0) 0))
;         ))

;;; Check rows and columns still within square.
(define (outsideSquare original current)
  (or
   (< current (reduceSquareRootNumber original))
   (> current (+ (reduceSquareRootNumber original) 2))))

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
;         removeNumberColumnSet
         removeNumberRow
;         removeNumberRowSet
         removeNumberSquare
;         removeNumberSquareSet
         returnUniqueListRow
         setReplace
         transform
         untransform)
