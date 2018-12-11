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
;;; The missing part of Martin's Fibonacci algorithm is the rule for Fib(1) and Fib(0). both of these are defined to be 1.
;;; Using this information, write a correct version of the FIB function. (FIB 4) should return five. (FIB 5) should return eight.
(defun fib (n)
  (if (or (zerop n) (eq n 1)) 
      1
      (+ (fib (- n 1)) (fib (- n 2))))) 

;;; Ex 8.17
;;; Use double-test tail recursion to write FIND-FIRST-ODD, a function that returns the first odd number in a list, or NIL if there are none. Start by copying the recursion template values for ANYODDP; only a small change is necessary to derive FIND-FIRST-ODD.
(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

(defun find-first-odd (x)
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (find-first-odd (rest x)))))

;;; Ex 8.18
;;; Use single-test tail recursion to write LAST-ELEMENT, a function that returns the last element of a list.
;;; LAST-ELEMENT should recursively travel down the list until it reaches the last cons cell (a cell whose cdr is an atom); then it should return the car of this cell.
(defun last-element (x)
  (cond ((atom (cdr x)) (car x))
        (t (last-element (rest x)))))


;;; Ex 8.21
;;; Write a recursive function ADD-NUMS that add up the numbers N, N-1, N-2 and so on, down to 0, and returns the result.
;;; For example, (ADD-NUMS 5) should compute 5+4+3+2+1+0, which is 15.
(defun add-nums (x)
  (cond ((zerop x) 0)
        (t (+ x (add-nums (- x 1))))))

;;; Ex 8.22
;;; Write a recursive function ALL-EQUAL that returns T if the first element of a list is equal to the second, the second is equal to the third, the third is equal to the fourth, and so on. (ALL-EQUAL '(I I I I)) shoul return T.
;;; (ALL-EQUAL '(I I E I)) should return NIL. ALL-EQUAL should return T for lists with less than two elements.
(defun all-equal (l)
  (cond ((atom (cdr l)) t)
        (t (and (eq (first l) (second l)) (all-equal (rest l)))))) 

;;; Ex 8.24
;;; Write COUNT-DOWN, a function that counts down from n using list-counsing recursion.
;;; (COUNT-DOWN 5) should produce the list (5 4 3 2 1).
(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))

;;; Ex 8.26 
;;; Suppose we wanted to modify COUNT-DOWN so that the list it constructs ends in zero. 
;;; For example, (COUNT-DOWN 5) would produce (5 4 3 2 1 0). Show two ways this can be done.
(defun count-down (n)
  (cond ((zerop n) (cons n nil))
        (t (cons n (count-down (- n 1))))))

(defun count-down (n)
  (cond ((zerop n) (list n))
        (t (cons n (count-down (- n 1))))))

;;; Ex 8.27
;;; Write SQUARE-LIST, a recursive function that takes a list of numbers as input and returns a list of their squares. (SQUARE-LIST '(3 4 5 6)) should return (9 16 25 36).
(defun square-list (l)
  (cond ((null l) nil)
        (t (cons (* (car l) (car l)) (square-list (rest l))))))
