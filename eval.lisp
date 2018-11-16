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

;; Ex 3.22 d
; Write a predicate FIRSTP that returns T if its first argument (a symbol) is equal to the first element of its second argument (a list).
; That is, (FIRSTP 'FOO '(FOO BAR BAZ)) should return T.
; (FIRSTP 'BOING '(FOO BAR BAZ)) should return NIL.
(defun firstp (s my-list)
  (equal s (car my-list)))  

;; Ex 3.22 e
; Write a function MID-ADD1 that adds 1 to the middle element of a three element list. For example, (MID-ADD1 '(TAKE 2 COOKIES)) should return the list (TAKE 3 COOKIES).
; Note: You are not allowed to make MID-ADD1 a function of three inputs. It has to take a single input that is a list of three elements.
(defun mid-add1 (my-list)
  (list (first my-list) (+ (second my-list) 1) (third my-list)))

;; Ex 3.22 f
; Write a function F-TO-C that converts a temperature from Fahrenheit to Celsius. The formula for doing the conversion is: Celsius temperature = [5X(Fahrenheit temperature - 32)]/9.
; To go in the opposite direction, the formula is: Fahrenheit temperature = (9/5X Celsius temperature) + 32.
(defun f-to-c (temp)
  (/ (* (- temp 32) 5) 9))

;; Ex 3.23
; Write each of the following functions in Church's lambda notation: DOUBLE, SQUARE, ONEMOREREP
(defun my-double (n)
  (* n 2))

(defun my-square (n)
  (* n n))

(defun onemorerep (n)
  (1+ n))
