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
