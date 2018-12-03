;;; Chapter 7 - Applicative Programming
;;; Exercises

;;; Ex 7.1
;;; Write an ADD1 function that adds one to its input. Then write an expression to add one to each of the list (1 3 5 7 9)
(defun add1 (n)
  (+ n 1))

(mapcar #'add1 '(1 3 5 7 9))

;;; Ex 7.2
let the global variable DAILY-PLANET contain the following table:
((olsen jimmy 123-76-4535 cub-reporter)
 (kent  clark 089-52-6787 reporter)
 (lane  lois  951-26-1438 reporter)
 (white perry 355-16-7439 editor))
;;; Each table entry consist of a last name, a first name, a social security number, and a job title. use MAPCAR on this table to extract a list of social numbers.
(setf daily-planet 
     '((olsen jimmy 123-76-4535 cub-reporter)
       (kent  clark 089-52-6787 reporter)
       (lane  lois  951-26-1438 reporter)
       (white perry 355-16-7439 editor)))

(mapcar #'third daily-planet)

;;; Write an expression to apply the ZEROP predicate to each element of the list (2 0 3 4 0 -5 -6).
;;; The answer you get should be a list of Ts and NILs.
(mapcar #'zerop '(2 0 3 4 0 -5 -6))

;;; Suppose we want to solve a problem similar to the preceding one, but instead of testing wheter an element is zero, we want to test whether it is greater than five.
;;; We can't use > directly for this because > is a function of two inputs; MAPCAR  will only give it one input.
;;; Show how first writing a one-input function called GREATER-THAN-FIVE-P would help.
(defun greater-than-five-p (n)
  (> n 5))

(mapcar #'greater-than-five-p '(2 0 3 4 0 -5 -6))

;;; Ex 7.5
;;; Write a lambda expression to subtract seven from a number
#'(lambda (n) (- n 7))

;;; Ex 7.6
;;; Write a lambda expression that returns T if its input is T or NIL, but NIL for any other input.
#'(lambda (i) (cond (or (eq t i) (eq nil i)) t))



