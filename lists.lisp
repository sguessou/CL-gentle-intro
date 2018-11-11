;;;; CL, Gentle Introduction to symbolic computation 
;;; Chap. 2 exercises

;; Ex 2.8
; Show how to write MY-THIRD using FIRST ans two RESTs.
(defun my-third (a-list)
  (first (rest (rest a-list))))

;; Ex 2.9
; Show how to write MY-THIRD using SECOND.
(defun my-third-b (a-list)
  (second (rest a-list)))

;; Ex 2.13
; Write the functions to get each word in the list: (((FUN)) (IN THE) (SUN))
(defun get-first (my-list)
  (caaar my-list))

(defun get-second-first (my-list)
  (caadr my-list))

(defun get-second-second (my-list)
  (cadadr my-list))

(defun get-third (my-list)
  (caaddr my-list))

;; Ex 2.18
; Write a function that takes any two inputs and make a list of them using CONS
(defun generate-list (a b)
  (cons a (cons b nil)))

;; Ex 2.21
; Write a function that takes four inputs and returns a two-element nested list. 
; The first element should be a list of the two inputs, and the second element a list of the last two inputs.
(defun my-four-list (a b c d)
  (list (list a b) (list c d)))

;; Ex 2.22
; Suppose we wanted to make a function called DUO-CONS that added two elements to the front of a list. Remember that the regular CONS function adds only one element to a list.
; DUO-CONS would be a function of three inputs. For example, if the inputs were the symbol PATRICK, the symbol SEYMOUR, and the list (MARVIN), DUO-CONS would return the list (PATRICK SEYMOUR MARVIN).
; Show how to write the DUO-CONS function.
(defun duo-cons (a b alist)
  (cons a (cons b alist)))  

;;; Ex 2.23 
;; TWO-DEEPER is a function that surrounds its input with two level of parentheses.
;; TWO-DEEPER of MOO is ((MOO)). TWO-DEEPER of (BOW WOW) is (((BOW WOW))).
;; Show how to write TWO-DEEPER using list. Write another version using CONS.
; LIST version:
(defun two-deeper (el)
  (list (list el)))
; CONS version:
(defun two-deeper (el)
  (cons (cons el nil) nil))

;; Ex 2.24
; What built-in Lisp function would extract the symbol NIGHT from the list (((GOOD)) ((NIGHT)))?
(caaadr '(((GOOD)) ((NIGHT))))
 
;; Ex 2.29
; Write a function UNARY-ADD1 that increases a unary number by one.
(defun unary-add1 (n)
  (cons 'x n))

;; Ex 2.31
; Write a UNARY-ZEROP predicate
(defun unary-zerop (n)
  (eq (length n) 0))

;; Ex 2.32
; Write a UNARY-GREATERP predicate, analogous to the > predicate on ordinary numbers.
(defun unary-greater (n m)
  (> (length n) m))

;; Ex 2.34
; Write an expression involving cascaded calls to CONS to construct the dotted list (A B C . D).
(defun my-func ()
  (cons 'a (cons 'b (cons 'c 'd))))

;; Ex 2.35
; Write an expression to construct this list: ((A . B) (C . D)).
(defun my-cons ()
  (list (cons 'a 'b) (cons 'c 'd)))
