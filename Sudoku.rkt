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
;;; Returns a list.
(define (setReplace digit)
  (if (= 0 digit)
      (list 1 2 3 4 5 6 7 8 9)
      (list digit)
      )
  )

;;; Replaces the list with a flattened list.
;;; Returns either a '0' or a number for that Sudoku cell.
(define (unReplace list)
  (if (> (length list) 1)
      0
      (list-ref (flatten list) 0)
      )
  )

;;; Transforms the matrix into a list of lists of sets.
;;; Returns a matrix.
(define (transform matrix)
  (map (lambda (x) (map setReplace x)) matrix))

;;; Returns the matrix back to standard form.
;;; Returns a matrix.
(define (untransform matrix)
  (map (lambda (x) (map unReplace x)) matrix))

;;;
;;;  COUNTING FUNCTION
;;;

;;; Count at the matrix level.
;;; Returns an integer.
(define (countNonSingletons matrix)
  (foldl + 0 (map (lambda (x) (if (eq? (checkSingleton x) #f) 1 0)) (apply append matrix))))

;;; Check is singleton or not.
;;; Returns a boolean.
(define (checkSingleton set)
  (if (= (length set) 1)
      #t
      #f
      )
  )

;;;
;;;  THE SOLVER
;;;

;;; The solve function.
;;; Returns a matrix.
(define (solve matrix)
  (untransform (loopMatrix (transform matrix) 0 0))
  )

;;;
;;;  MAIN ALGORITHM
;;;

;;; Loop through the matrix applying the algorithm.
;;; Returns a matrix.
(define (loopMatrix matrix r c)
  (if (< (countNonSingletons matrix) 1)
      matrix
      (if (< c 9)
          (loopMatrix (applyAlgorithm matrix r c) r (+ c 1))
          (if (< r 8)
              (loopMatrix matrix (+ r 1) 0)
              (loopMatrix matrix 0 0)
              )
          )
      )
  )

;;; Apply the algorithm.
(define (applyAlgorithm matrix row col)
  (let ([digit (list-ref (list-ref matrix row) col)])
    (if (checkSingleton digit)
        (singletonTransform matrix row col digit)
        (nonSingletonTransform matrix row col digit)
        )
    )
  )

;;; Perform all checks and actions when a singleton is identified.
(define (singletonTransform matrix row col digit)
  (removeNumberSquare
   (removeNumberColumn
    (removeNumberRow matrix row (first digit))
    col (first digit))
   row col (first digit)
   )
  )

;;; Perform all checks and actions when a singleton is not identified.
(define (nonSingletonTransform matrix row col numbers)
  (let ([y (combineNonDupeLists matrix row col)])
    (let ([z (list-set matrix row
              (list-set (list-ref matrix row) col
                        (let ([filteredResult (filter (lambda (x) (not (member x y))) numbers)])
                          (if (checkSingleton filteredResult)
                              filteredResult
                              numbers
                              )
                          )
                        )
              )]
          )
      z
      )
    )
  )

;;; Remove digit from row.
;;; Returns a row as a list.
(define (removeNumberRow matrix row digit)
  (let ([x (list-ref matrix row)])
    (list-set matrix row (map (lambda (y) (removeNumber y digit)) x))))

;;; Create list of non-duplicates from surrounding cells.
;;; Returns a list. 
(define (combineNonDupeLists matrix row col)
  (sort
   (remove-duplicates
    (flatten
     (list
      (returnUniqueListRow matrix row col)
      ;(returnUniqueListCol matrix row col)
      ;(returnUniqueListSquare matrix row col)
      )
     )
    )
   <)
  )

;;; Remove duplicates from a row.
;;; Returns a list as a row.
(define (returnUniqueListRow matrix row col)
  (let ([x (list-ref matrix row)])
    (remove-duplicates
     (flatten
      (append
       (take x col)
       (drop x (+ 1 col))
       )
      )
     )
    )
  )

;;; Remove duplicates from a column.
(define (returnUniqueListCol matrix row col)
  (let ([C (map (lambda (x) (list-ref x col)) matrix)])
    (remove-duplicates
     (flatten
      (append
       (take C row)
       (drop C (+ 1 row))
       )
      )
     )
    )
  )

;;; Remove duplicates from a square.
(define (returnUniqueListSquare matrix row col)
  (remove-duplicates
   (flatten
    (returnNumberSquareSet matrix row col)
    )
   )
  )

;;; Remove the digit from column.
(define (removeNumberColumn matrix column digit)
  (map (lambda (x) (list-set x column (removeNumber (list-ref x column) digit))) matrix))

;;; Remove the digit from the square.
(define (removeNumberSquare matrix row col digit)
  (list
   (if (insideSquare 0 row) (updateRow (first matrix) col digit) (first matrix))
   (if (insideSquare 1 row) (updateRow (second matrix) col digit) (second matrix))
   (if (insideSquare 2 row) (updateRow (third matrix) col digit) (third matrix))
   (if (insideSquare 3 row) (updateRow (fourth matrix) col digit) (fourth matrix))
   (if (insideSquare 4 row) (updateRow (fifth matrix) col digit) (fifth matrix))
   (if (insideSquare 5 row) (updateRow (sixth matrix) col digit) (sixth matrix))
   (if (insideSquare 6 row) (updateRow (seventh matrix) col digit) (seventh matrix))
   (if (insideSquare 7 row) (updateRow (eighth matrix) col digit) (eighth matrix))
   (if (insideSquare 8 row) (updateRow (ninth matrix) col digit) (ninth matrix)))
  )

;;; Remove the digit from the square.
(define (updateRow matrix col digit)
  (list
   (if (insideSquare 0 col) (removeNumber (first matrix) digit) (first matrix))
   (if (insideSquare 1 col) (removeNumber (second matrix) digit) (second matrix))
   (if (insideSquare 2 col) (removeNumber (third matrix) digit) (third matrix))
   (if (insideSquare 3 col) (removeNumber (fourth matrix) digit) (fourth matrix))
   (if (insideSquare 4 col) (removeNumber (fifth matrix) digit) (fifth matrix))
   (if (insideSquare 5 col) (removeNumber (sixth matrix) digit) (sixth matrix))
   (if (insideSquare 6 col) (removeNumber (seventh matrix) digit) (seventh matrix))
   (if (insideSquare 7 col) (removeNumber (eighth matrix) digit) (eighth matrix))
   (if (insideSquare 8 col) (removeNumber (ninth matrix) digit) (ninth matrix)))
  )

;;; Return a list of numbers from an internal square.
(define (returnNumberSquareSet matrix row col)
  (list
   (if (insideSquare 0 row) (returnRowSquare (first matrix) (= 0 row) col) '())
   (if (insideSquare 1 row) (returnRowSquare (second matrix) (= 1 row) col) '())
   (if (insideSquare 2 row) (returnRowSquare (third matrix) (= 2 row) col) '())
   (if (insideSquare 3 row) (returnRowSquare (fourth matrix) (= 3 row) col) '())
   (if (insideSquare 4 row) (returnRowSquare (fifth matrix) (= 4 row) col) '())
   (if (insideSquare 5 row) (returnRowSquare (sixth matrix) (= 5 row) col) '())
   (if (insideSquare 6 row) (returnRowSquare (seventh matrix) (= 6 row) col) '())
   (if (insideSquare 7 row) (returnRowSquare (eighth matrix) (= 7 row) col) '())
   (if (insideSquare 8 row) (returnRowSquare (ninth matrix) (= 8 row) col) '())
   )
  )

;;; Return a list of numbers from a row.
;;; Returns a list.
(define (returnRowSquare rowList rowBool col)
  (list
   (if (insideSquare 0 col) (if (and rowBool (= col 0)) '() (first rowList)) '())
   (if (insideSquare 1 col) (if (and rowBool (= col 1)) '() (second rowList))'())
   (if (insideSquare 2 col) (if (and rowBool (= col 2)) '() (third rowList)) '())
   (if (insideSquare 3 col) (if (and rowBool (= col 3)) '() (fourth rowList)) '())
   (if (insideSquare 4 col) (if (and rowBool (= col 4)) '() (fifth rowList)) '())
   (if (insideSquare 5 col) (if (and rowBool (= col 5)) '() (sixth rowList)) '())
   (if (insideSquare 6 col) (if (and rowBool (= col 6)) '() (seventh rowList)) '())
   (if (insideSquare 7 col) (if (and rowBool (= col 7)) '() (eighth rowList)) '())
   (if (insideSquare 8 col) (if (and rowBool (= col 8)) '() (ninth rowList)) '())
   )
  )

;;; Check rows and columns still within square.
;;; Returns true or false depending on whether current is within the square represented by original.
(define (insideSquare original current)
  (and
   (>= current (reduceSquareRootNumber original))
   (<= current (+ (reduceSquareRootNumber original) 2))
   )
  )

;;; Reduce to square root number.
;;; Returns the digit rounded down to 1st digit of enclosing square.
(define (reduceSquareRootNumber digit)
  (cond
    [(< digit 3) 0]
    [(< digit 6) 3]
    [(< digit 9) 6]
    )
  )

;;; Remove number from set.
;;; Returns the set passed or the set with the digit removed.
(define (removeNumber set digit)
  (if (eq? #f (checkSingleton set))
      (filter (lambda (x)  (not (= x digit))) set)
      set
      )
  )

;;;
;;;  PROVIDER FUNCTION
;;;

;;; Provides all functions for testing.
(provide checkSingleton
         applyAlgorithm
         combineNonDupeLists
         countNonSingletons
         insideSquare
         nonSingletonTransform
         reduceSquareRootNumber
         removeNumber
         removeNumberColumn
         removeNumberRow
         removeNumberSquare
         returnNumberSquareSet
         returnRowSquare
         returnUniqueListCol
         returnUniqueListRow
         returnUniqueListSquare
         setReplace
         transform
         untransform)
