;;; Chapter 3 - EVAL Notation
;; Exercises

;; Ex 3.6 
; Define a function PYTHAG that takes two input, x and y, and return the square root of x^2+y^2.
; (PYTHAG 3 4) should return 5.0.
(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))  

;; Ex 3.11
; Define a predicate called LONGER-THAN that takes two lists as input and returns T if the first list is longer than the second.
(defun longer-than (a b)
  (> (length a) (length b)))

;; Ex 3.12
; Write a function ADDLENGTH that takes a list as input and returns a new list with the length of the input added onto the front of it. If the input is (MOO GOO GAI PAN),
; the output should be (4 MOO GOO GAI PAN).
(defun addlength (my-list)
  (cons (length my-list) my-list))
