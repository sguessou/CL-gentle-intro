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

;;; Ex 7.7
;;; Write a function that takes a list such as (UP DOWN UP UP) and "flips" each element, returning (DOWN UP DOWN DOWN). 
;;; Your function should include a lambda expression that knows how to flip an individual element, plus an applicative operator to do this to every element of the list.
(defun flip (l)
  (mapcar #'(lambda (e) 
              (if (eq e 'up) 
                  'down
                  'up)) l))

;;; Ex 7.8
;;; Write a function that takes two inputs, X and K, and returns the first number in the list X that is roughly equal to K.
;;; Let's say that "roughly equal" means no less than K-10 and no more than K+10.
(defun ex-7.8 (x k)
  (find-if #'(lambda (e)
               (and (>= e (- k 10)) (<= 2 (+ k 10))))
           x))

;;; Ex 7.9
;;; Write a function FIND-NESTED that returns the first element of a list that is itself a non-NIL list.
(defun find-nested (l)
  (find-if #'(lambda (e)
                  (and (listp e) (> (length e) 0)))
              l))

(defun find-nested (l)
  (find-if #'consp l))

;;; Ex 7.10
;;; In this exercise we will write a program to transpose a song from one key to another. In order to manipulate notes more efficiently, we will translate them into numbers.
;;; Here is the correspondence between notes and numbers for a non-octave scale:
C       = 1         F-SHARP = 7
C-SHARP = 2         G       = 8
D       = 3         G-SHARP = 9
D-SHARP = 4         A       = 10
E       = 5         A-SHARP = 11
F       = 6         B       = 12
;;; a.
;;; Write a table to represent this information. Store it in a global variable called NOTE-TABLE.
(setf note-table 
      '((c 1) (c-sharp 2) (d 3) (d-sharp 4) (e 5) (f 6)
        (f-sharp 7) (g 8) (g-sharp 9) (a 10) (a-sharp 11)
        (b 12)))
    
;;; b.
;;; Write a function called NUMBERS that takes a list of notes as input and returns the corresponding list of numbers. (NUMBERS '(E D C D E E E)) should return (5 3 1 3 5 5 5).
;;; This list represents the first seven notes of "Mary Had a Little Lamb."
(defun numbers (l)
  (mapcar #'(lambda (x)
              (cadr (assoc x note-table)))
          l))

;;; c.
;;; Write a function called NOTES that takes a list of numbers as input and returns the corresponding list of notes. (NOTES '(5 3 1 3 5 5 5)) should return (E D C D E E E). Hint: Since NOTE-TABLE is keyed by note, ASSOC can't look up numbers in it;
;;; neither can RASSOC, since the elements are lists, not dotted pairs. Write you own table-searching function to search NOTE-TABLE by number instead of by note.
(defun table-searching (x)
  (find-if #'(lambda (e)
              (eq (car (reverse e)) x))
           note-table))

(defun notes (l)
  (mapcar #'(lambda (x)
              (car x))
          (mapcar #'table-searching l)))

;;; To transpose a piece of music up by n half steps, we begin by adding the value n to each note in the piece. Write a function called RAISE that takes a number n and a list of numbers as input and raises each number in the list by the value n.
;;; (RAISE 5 '(5 3 1 3 5 5 5)) should return (10 8 6 8 10 10 10), whis is "Mary had a little lamb" transposed five half steps from the key of C to the key of F.
(defun raise (n l)
  (mapcar #'(lambda (x)
              (+ x n))
          l))
