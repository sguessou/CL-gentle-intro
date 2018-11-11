;;; Chapter 3 - EVAL Notation
;; Exercises

;; Ex 3.6 
; Define a function PYTHAG that takes two input, x and y, and return the square root of x^2+y^2.
; (PYTHAG 3 4) should return 5.0.
(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))  

