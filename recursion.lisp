;;; Chapter 8 - Recursion
;;; Exercises

;;; Ex 8.2
;;; Show how to write ANYODDP using IF instead of COND
(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

(defun anyoddp (x)
  (if (null x)
      nil
      (if (oddp (first x))
          t
          (anyoddp (rest x)))))

(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))

;;; Ex 8.4
;;; We are going to write a function called LAUGH that takes a number as input and returns a list of that many HAs.
;;; (LAUGH 3) should return the list (HA HA HA). (LAUGH 0) should return a list with no HAs in it.
(defun laugh (n)
  (cond ((or (zerop n) (< n 0)) nil)
        (t (cons 'ha (laugh (- n 1))))))


;;; Ex 8.5 
;;; In this exercise we are going to write a function ADD-UP to add up all the numbers in a list. (ADD-UP '(2 3 7)) should return 12.
;;; You already known how to solve this problem applicatively with REDUCE; now you'll learn to solve it recursively.
;;; Write down the complete definition of ADD-UP. Type it into the computer and then try adding up a list of numbers.
(defun add-up (l)
  (cond ((null l) 0)
        (t (+ (first l) (add-up (rest l))))))

;;; Ex 8.6
;;; Write ALLODDP, a recursive function that returns T if all the numbers in a list are odd.
(defun alloddp (l)
  (cond ((or (null l)) t)
        (t (and (oddp (first l)) (alloddp (rest l))))))

;;; Ex 8.7
;;; Write a recursive version of MEMBER. Call it REC-MEMBER so you don't redefine the built-in MEMBER function.
(defun rec-member (n l)
  (cond ((null l) nil)
        (t (if (eq n (first l))
            (append l (rec-member n nil))
            (rec-member n (rest l))))))

;;; Ex 8.8
;;; Write a recursive version of ASSOC. Call it REC-ASSOC.
(defun rec-assoc (n l)
  (cond ((null l) nil)
        (t (if (eq n (caar l))
               (append (car l) (rec-assoc n nil))
               (rec-assoc n (rest l))))))

;;; Ex 8.9
;;; Write a recursive function of NTH. Call it REC-NTH.
(defun rec-nth (n l)
  (cond ((or (null l) (< n 0)) nil)
        (t (if (zerop n)
               (first l)
               (rec-nth (- n 1) (rest l))))))

;;; Ex 8.10 
;;; For x a nonnegative integer and y a positive integer, x+y equals x+1+(y-1). If y is zero then x+y equals x. Use these equations to build a recursive version of + called REC-PLUS out of ADD1, SUB1, COND and ZEROP.
;;; You'll have to write ADD1 and SUB1 too.
(defun add1 (x)
  (+ x 1))

(defun subb1 (x)
  (- x 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (subb1 y)))))

;;; Ex 8.11
;;; The missing part of Martin's Fibonacci algorithm is the rule for Fib(1) and Fib(o). both of these are defined to be 1.
;;; Using this information, write a correct version of the FIB function. (FIB 4) should return five. (FIB 5) should return eight.
(defun fib (n)
  (if (or (zerop n) (eq n 1)) 
      1
      (+ (fib (- n 1)) (fib (- n 2))))) 

