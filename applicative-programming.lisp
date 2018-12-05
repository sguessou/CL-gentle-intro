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

;;; e.
;;; To transpose a piece of music up by n half steps, we begin by adding the value n to each note in the piece. Write a function called RAISE that takes a number n and a list of numbers as input and raises each number in the list by the value n.
;;; (RAISE 5 '(5 3 1 3 5 5 5)) should return (10 8 6 8 10 10 10), whis is "Mary had a little lamb" transposed five half steps from the key of C to the key of F.
(defun raise (n l)
  (mapcar #'(lambda (x)
              (+ x n))
          l))

;;; f.
;;; Sometimes when we raise the value of a note, we may raise it right into the next octave. For instance, if we raise the triad C-E-G represented by the list (1 5 8) into the key of F by adding five to each note, we get (6 10 13), or F-A-C.
;;; Here the C note, represented by the number 13, is an octave above the regular C, represented by 1. Write a function called NORMALIZE that takes a list of numbers as input and "normalizes" them to make them be between 1 and 12.
;;; A number greater than 12 should have 12 subtracted from it; a number less than 1 should have 12 added to it. (NORMALIZE '(6 10 13)) should return (6 10 1).
(defun normalize (l)
  (mapcar #'(lambda (x)
              (cond ((> x 12) (- x 12))
                    ((< x 1) (+ x 12))
                    (t x)))
          l))

;;; g.
;;; Write a function TRANSPOSE that takes a number n and a song as input, and returns the song transposed by n half steps.
;;; (TRANSPOSES 5 '(E D C D E E E)) should return (A G F G A A A). Your solution should assume the availability of the NUMBERS, NOTES, RAISE and NORMALIZE functions. Try transposing "Mary Had a Little Lamb" up by 11 half steps. What happens if you transpose it by 12 half steps? How about -1 half steps?
(defun transpose (n l)
  (notes 
   (normalize 
    (raise n (numbers l)))))

;;; Ex 7.11
;;; Write a function to pick out those numbers in a list that are greater than one and less than five.
(defun ex-711 (l)
  (remove-if-not #'(lambda (x)
                     (and (> x 1) (< x 5)))
                 l))

;;; Ex 7.12
;;; Write a function that counts how many times the word "the" appears in a sentence.
(defun ex-712 (l)
  (length (remove-if-not #'(lambda (x)
                             (eq x 'the))
                         l)))
