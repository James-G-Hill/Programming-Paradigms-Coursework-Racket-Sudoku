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
         removeNumber
         removeNumberRow
         setReplace
         transform)
