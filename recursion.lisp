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

;;; Ex 8.25
;;; How could COUNT-DOWN be used to write an applicative version of FACT?
(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))

(defun fact (n)
  (reduce #'(lambda (x y) (* x y))
          (count-down n)))

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

;;; Ex 8.27
;;; The expressions (MY-NTH 5 '(A B C)) and (MY-NTH 1000 '(A B C)) both run off the end of the list, and hence produce a NIL result.
;;; Yet the second expression takes quite a bit longer to execute than the first.
;;; Modify MY-NTH so that the recursion stops as soon the function runs off the end of the list.
(defun my-nth (n x)
  (cond ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))

(defun my-nth (n x)
  (cond ((or (zerop n) (atom (cdr x))) (first x))
        (t (my-nth (- n 1) (rest x)))))

;;; Ex 8.29
;;; Write MY-MEMBER, a recursive version of MEMBER. This function will take two inputs, but you will only want to reduce one of them with each successive call.
;;; The other should remain unchanged.
(defun my-member (x l)
  (cond ((null l) nil)
        ((eq (first l) x) l)
        (t (my-member x (rest l)))))

;;; Ex 8.30
;;; Write MY-ASSOC, a recursive version of ASSOC.
(defun my-assoc (x l)
  (cond ((null l) nil)
        ((member x (first l)) (first l))
        (t (my-assoc x (rest l)))))

;;; Ex 8.31
;;; Suppose we want to tell as quickly as possible whether one list is shorter than another. If one list has five elements and the other has a million, we don't want to have to go through all one million cons cells before deciding that the second list is longer.
;;; So we must not call LENGTH on the two lists. Write a recursive function COMPARE-LENGTH that takes two lists as input and returns one of the following symbols:
;;; SAME-LENGTH, FIRST-IS-LONGER, or SECOND-IS-LONGER.
;;; Use triple-test simultanous recursion.
(defun compare-length (m n)
  (cond ((and (null m) (null n)) 'same-length)
        ((null m) 'second-is-longer)
        ((null n) 'first-is-longer)
        (t (compare-length (rest m) (rest n)))))

;;; Ex 8.32
;;; Write the function SUM-NUMERIC-ELEMENTS, which adds up all the numbers in a list and ignores the non-numbers. (SUM-NUMERIC-ELEMENTS '(3 BEARS 3 BOWLS AND 1 GIRL)) should return seven.
(defun sum-numeric-elements (l)
  (cond ((null l) 0)
        ((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
        (t (sum-numeric-elements (rest l)))))

;;; Ex 8.33
;;; Write MY-REMOVE, a recursive version of the REMOVE function.
(defun my-remove (s l)
  (cond ((null l) nil)
        ((eq s (first l)) (my-remove s (rest l)))
        (t (cons (first l) (my-remove s (rest l))))))

;;; Ex 8.34
;;; Write MY-INTERSECTION, a recursive version of the INTERSECTION function.
(defun my-intersection (a b)
  (cond ((or (null a) (null b)) nil)
        ((member (first a) b) 
         (cons (first a) 
               (my-intersection (rest a) (remove (first a) b))))
        (t (my-intersection (rest a) b))))

;;; Ex 8.35
;;; Write MY-SET-DIFFERENCE, a recursive version of the SET-DIFFERENCE function.
(defun my-set-difference (a b)
  (cond ((or (null a) (null b)) nil)
        ((not (member (first a) b))  
         (cons (first a) 
               (my-set-difference (rest a) (remove (first a) b))))
        (t (my-set-difference (rest a) b))))

;;; Ex 8.36
;;; The function COUNT-ODD counts the number of odd elements in a list of numbers; for example, (COUNT-ODD '(4 5 6 7 8)) should return two.
;;; Show how to write COUNT-ODD using conditional augmentation.
;;; Then write another version of COUNT-ODD using the regular augmenting recursion template.
(defun count-odd (l)
  (cond ((null l) 0)
        ((oddp (first l)) (+ 1 (count-odd (rest l))))
        (t (count-odd (rest l))))) 

(defun count-odd (l)
  (cond ((null l) 0)
        (t (+ (if (oddp (first l)) 1 0) (count-odd (rest l))))))

;;; Ex 8.37
;;; Define a simple function COMBINE that takes two numbers as input and returns their sum. Now replace the occurence of + in FIB with COMBINE. Trace FIB and COMBINE. Trace FIB and COMBINE, and try evaluating (FIB 3) or (FIB 4). What can you say about the relationship between COMBINE, terminal calls, and nonterminal calls?
(defun combine (a b)
  (+ a b))

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (combine (fib (- n 1)) (fib (- n 2))))))

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

;;; Ex 8.39
;;; Write a function COUNT-ATOMS that returns the number of atoms in a tree. 
;;; (COUNT-ATOMS '(A (B) C)) should return five, since in addition to A, B, and C there are two NILS in the tree.
(defun count-atoms (l)
  (cond ((atom l) 1)
        (t (+ (count-atoms (car l)) (count-atoms (cdr l))))))

;;; Ex 8.40
;;; Write COUNT-CONS, a function that returns the number of cons cells in a tree.
;;; (COUNT-CONS '(FOO)) should return one. (COUNT-CONS '(FOO BAR)) should return two. (COUNT-CONS '((FOO))) should also return two, since the list ((FOO)) requires two cons cells.
;;; (COUNT-CONS 'FRED) should return zero.
(defun count-cons (l)
  (cond ((atom l) 0)
        (t (+ 1 (count-cons (car l)) (count-cons (cdr l))))))

;;; Ex 8.41
;;; write a function SUM-TREE that returns the sum of all the numbers appearing in a tree. Nonnumbers should be ignored.
;;; (SUM-TREE '((3 BEARS) (3 BOWLS) (1 GIRL))) should return seven.
(defun sum-tree (l)
  (cond ((or (null l) (symbolp l)) 0)
        ((numberp l) l)
        (t (+ (sum-tree (car l)) (sum-tree (cdr l))))))

;;; Ex 8.42
;;; Write MY-SUBST, a recursive version of the SUBST function.
(defun my-subst (n o l)
  (cond ((eq l o) n)
        ((atom l) l)
        (t (cons (my-subst n o (car l)) (my-subst n o (cdr l))))))

;;; Ex 8.43
;;; Write FLATTEN, a function that returns all the element of an arbitrarily nested list in a single-level list.
;;; (FLATTEN '((A B (R)) A C (A D ((A (B)) R) A))) should return (A B R A C A D A B R A).
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (append (flatten (car l)) (flatten (cdr l))))))

;;; Ex 8.44
;;; Write a function TREE-DEPTH that returns the maximum depth of a binary tree.
;;; (TREE-DEPTH '(A . B)) should return one. 
;;; (TREE-DEPTH '((A B C D))) should return five and (TREE-DEPTH '((A . B).(C . D))) should return two.
(defun tree-depth (l)
  (cond ((atom l) 0)
        (t (+ 1 (max (tree-depth (car l)) (tree-depth (cdr l)))))))

;;; Ex 8.45
;;; Write a function PAREN-DEPTH that returns the maximum depth of nested parentheses in a list. (PAREN-DEPTH '(A B C)) should return one, whereas TREE-DEPTH would return three.
;;; (PAREN-DEPTH '(A B ((C) D) E)) should return three, since there is an element C that is nested in three level of parentheses.
(defun paren-depth (l)
  (cond ((atom l) 0)
        (t (max (+ 1 (paren-depth (car l))) (paren-depth (cdr l))))))

;;; Ex 8.46
;;; Another way to solve the problem of counting upward is to add an element to the end of the list with each recursive call instead of adding elements at the beginning.
;;; This approach doesn't require a helping function. Write this version of COUNT-UP.
(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up-recursively  (cnt n)
  (cond ((> cnt n) nil)
        (t (cons cnt
                 (count-up-recursively
                  (+ cnt 1) n)))))

(defun count-up (n)
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1)) (list n)))))

;;; Ex 8.47
;;; Write MAKE-LOAF, a function that returns a loaf of size N.
;;; (MAKE-LOAF 4) should return (X X X X). Use IF instead of COND.
(defun make-loaf (n)
  (if (zerop n)
      nil
      (append (make-loaf (- n 1)) (list 'X))))

;;; Ex 8.48
;;; Write a recursive function BURY that buries an item under n levels of parentheses. (BURY 'FRED 2) should return ((FRED)), while (BURY 'FRED 5) should return (((((FRED))))).
;;; Which recursion template did you use?
(defun bury (s n)
  (cond ((zerop n) s)
        (t (bury (list s) (- n 1)))))
