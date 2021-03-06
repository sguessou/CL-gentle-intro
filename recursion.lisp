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

;;; Ex 8.49
;;; Write PAIRING, a function that pairs the elements of two lists.
;;; (PAIRING '(A B C) '(1 2 3)) should return ((A 1) (B 2) (C 3)).
;;; You may assume that the two lists will be of equal length.
(defun pairing (a b)
  (cond ((null a) nil)
        (t (cons (list (first a) (first b)) 
                   (pairing (rest a) (rest b)))))) 

;;; Ex 8.50
;;; Write SUBLISTS, a function that returns the successive sublist of a list.
;;; (SUBLIST '(FEE FIE FOE)) should return ((FEE FIE FOE) (FIE FOE) (FOE)).
(defun sublists (l)
  (cond ((null l) nil)
        (t (cons l (sublists (rest l))))))

;;; Ex 8.51
;;; The simplest way to write MY-REVERSE, a recursive version of REVERSE, is with a helping function plus a recursive function of two inputs.
;;; Write this version of MY-REVERSE.
(defun my-reverse (l)
  (cond ((null l) nil)
        (t (append (last l) (my-reverse (butlast l))))))

;;; Ex 8.52
;;; Write MY-UNION, a recursive version of UNION.
(defun my-union (a b)
  (cond ((null a) b)
        (t (append (list (first a))
                   (my-union (rest a)
                             (remove (first a) b))))))

;;; Ex 8.53
;;; Write LARGEST-EVEN, a recursive function that returns the largest even number in a list of nonnegative integers.
;;; (LARGEST-EVEN '(5 2 4 3)) should return four.
;;; (LARGEST-EVEN NIL) should return zero.
;;; Use the built-in MAX function, which returns the largest of its inputs.
(defun largest-even (l)
  (cond ((null l) 0)
        (t (max (if (evenp (first l)) 
                    (first l) 0)
                (largest-even 
                 (rest l))))))

;;; Ex 8.54
;;; Write a recursive function HUGE that raises a number to its own power. (HUGE 2) shoul return 4, (HUGE 3) should return 27, and so on.
;;; Do not use REDUCE.
(defun huge-helper (x n)
  (cond ((zerop n) 1)
        (t (* x (huge-helper x (- n 1))))))

(defun huge (n)
  (huge-helper n n))

;;; Ex 8.56
;;; Write EVERY-OTHER, a recursive function that returns every other element of a list--the first, third, fifth, and so on. 
;;; (EVERY-OTHER '(A B C D E F G)) should return (A C E G).
;;; (EVERY-OTHER '(I CAME I SAW I CONQUERED)) should return (I I I).
(defun every-other-helper (n l)
  (cond ((null l) nil)
        (t (append (if (oddp n) 
                       (list (first l))
                       nil)
                   (every-other-helper (+ n 1) (rest l))))))

(defun every-other (l)
  (every-other-helper 1 l))

;;; Ex 8.57
;;; Write LEFT-HALF, a recursive function in two parts that returns the first n/2 element of a list of length n.
;;; (LEFT-HALF '(A B C D E)) should return (A B C).
;;; (LEFT-HALF '(1 2 3 4 5 6 7 8)) should return (1 2 3 4).
;;; You may use LENGTH but not REVERSE in your definition.
(defun left-half-helper (n l)
  (cond ((zerop n) (list (first l)))
        (t (append (list (first l))
                   (left-half-helper (- n 1) (rest l))))))

(defun left-half (l)
  (left-half-helper (round (/ (length l) 2)) l))

;;; Ex 8.58
;;; Write MERGE-LISTS, a function that takes two lists of numbers, each in increasing order, as input. The function should return a list that is a merger of the elements in its inputs, in order.
;;; (MERGE-LISTS '(1 2 6 8 10 12) '(2 3 5 9 13)) should return (1 2 2 3 5 6 8 9 10 12 13).
(defun merge-lists (a b)
  (cond ((null a) b)
        ((null b) a)
        ((and (null a) (null b) nil))
        (t (append (if (> (first a) (first b))
                       (list (first b) (first a))
                       (list (first a) (first b)))
                   (merge-lists (rest a) (rest b))))))

;;; Ex 8.60
;;; Each person in the database is represented by an entry of form 
;;; (name father mother).
;;; When someone's father or mother is unknown, a value of NIL is used.  
(setf family
     '((colin nil nil) (deirdre nil nil) (arthur nil nil)
       (kate nil nil) (frank nil nil) (linda nil nil)
       (suzanne colin deirdre) (bruce arthur kate) (charles arthur kate)
       (david arthur kate) (ellen arthur kate) (george frank linda)
       (hillary frank linda) (andre nil nil) (tamara bruce suzanne)
       (vincent bruce suzanne) (wanda nil nil) (ivan george ellen)
       (julie george ellen) (marie george ellen) (nigel andre hillary)
       (frederick nil tamara) (zelda vincent wanda) (joshua ivan wanda)
       (quentin nil nil) (robert quentin julie) (olivia nigel marie)
       (peter nigel marie) (erica nil nil) (yvette robert zelda)
       (diane peter erica)))

;;; a.
;;; Write the functions FATHER, MOTHER, PARENTS, and CHILDREN that returns a person's father, mother, a list of his or her known parents, and a list of his or her children, respectively.
;;; (FATHER 'SUZANNE) should return COLIN.
;;; (PARENTS 'SUZANNE) should return (COLIN DEIRDRE).
;;; (PARENTS 'FREDERICK) should return (TAMARA), since Frederick's father is unknown. (CHILDREN 'ARTHUR) should return the set (BRUCE CHARLES DAVID ELLEN).
;;; If any of these functions is given NIL as input, it should return NIL.
;;; This feature will be useful later when we write some recursive functions.
(defun father (n)
  (cadr (find-if #'(lambda (m)
               (eq (car m) n))
           family)))

(defun father (n)
  (cadr (assoc n family)))

(defun mother (n)
  (caddr (find-if #'(lambda (m)
               (eq (car m) n))
           family)))

(defun mother (n)
  (caddr (assoc n family)))

(defun parents (n)
  (remove nil (cdr (find-if #'(lambda (m)
              (eq (car m) n))
           family))))

(defun parents (n)
  (set-difference 
   (assoc n family)
   (list n)))

(defun children (n)
  (mapcar #'(lambda (p)
              (car p))
          (remove-if-not #'(lambda (m)
               (or (eq (cadr m) n) (eq (caddr m) n)))
           family)))

;;; b.
;;; Write SIBLINGS, a function that returns a list of a person's siblings, including genetic half-siblings.
;;; (SIBLINGS 'BRUCE) should return (CHARLES DAVID ELLEN).
;;; (SIBLINGS 'ZELDA) should return (JOSHUA).
(defun list-fathers-helper (f)
  (cond ((null f) nil)
        (t (append (if (null (cadr (first f)))
                       nil
                       (list (cadr (first f))))
                   (list-fathers-helper (rest f))))))

(defun list-fathers (f)
  (remove-duplicates (list-fathers-helper f)))

(defun list-mothers-helper (f)
  (cond ((null f) nil)
        (t (append (if (null (caddr (first f)))
                       nil
                       (list (caddr (first f))))
                   (list-mothers-helper (rest f))))))

(defun list-mothers (f)
  (remove-duplicates (list-mothers-helper f)))

(defun list-parents (f)
  (union (list-fathers f) (list-mothers f)))

(defun siblings (n)
  (remove n (assoc n 
                   (remove-if-not #'(lambda (x)
                                      (> (length x) 1))
                                  (mapcar #'children 
                                          (list-parents family))))))


;;; simpler solution for siblings
(defun siblings (x)
  (set-difference (union (children (father x))
                         (children (mother x)))
                  (list x)))

;;; c.
;;; Write MAPUNION, an applicative operator that takes a function and a list as input, applies the function to every element of the list, and computes the union of all the results.
;;; An example is (MAPUNION #'REST '((1 A B C) (2 E C J) (3 F A B C D))), which should return the set (A B C E J F D). Hint: MAPUNION can be defined as a combination of two applicative operators you already know.
(defun mapunion (fn l)
  (reduce #'(lambda (x y)
              (union x y))
          (mapcar #'(lambda (x) (funcall fn x)) l)))

;;; d.
;;; Write GRANDPARENTS, a function that returns the set of a person's grandparents. Use MAPUNION in your solution.
(defun grandparents (fn n)
  (mapunion fn (funcall fn n)))

;;; e.
;;; Write COUSINS, a function that returns the set of a person's genetically related first cousins, in other words, the children of any of their parents' siblings.
;;; (COUSINS 'JULIE) should return the set (TAMARA VINCENT NIGEL).
;;; Use MAPUNION in your solution.
(defun cousins (n)
  (mapunion #'children
            (mapunion #'siblings 
                      (parents n)))) 

;;; f.
;;; Write the two-input recursive predicate DESCENDED-FROM that returns a true value if the first person is descended from the second.
;;; (DESCENDED-FROM 'TAMARA 'ARTHUR) should return T. (DESCENDED-FROM 'TAMARA 'LINDA) should return NIL.
;;; (Hint: You are descended from someone if he is one of your parents, or if either your father or mother is descended from him. This is a recursive definition.)
(defun descended-from (n m)
  (cond ((null (parents n)) nil)
        ((not (null (member m (parents n)))) t)
        (t (or (descended-from (father n) m) (descended-from (mother n) m)))))

;;; g.
;;; Write the recursive function ANCESTORS that returns a person's set of ancestors. (ANCESTORS 'MARIE) should return the set (ELLEN ARTHUR KATE GEORGE FRANK LINDA).
;;; (Hint: A person's ancestors are his parents plus his parents' ancestors. This is a recursive definition.)
(defun ancestors (n)
  (cond ((null (parents n)) nil)
        (t (append (parents n) (parents (father n)) (parents (mother n))))))

;;; h.
;;; Write the recursive function GENERATION-GAP that returns the number of generations separating a person and one of his or her ancestors.
;;; (GENERATION-GAP 'SUZANNE 'COLIN) should return one.
;;; (GENERATION-GAP 'FREDERICK 'COLIN) should return three.
;;; (GENERATION-GAP 'SUZANNE 'LINDA) should return NIL, because Linda is not an ancestor of Frederick.
(defun generation-gap (n m)
  (cond ((null (descended-from n m)) nil)
        ((member m (parents n)) 1)
        (t (if (null (descended-from (father n) m))
               (+ 1 (generation-gap (mother n) m))
               (+ 1 (generation-gap (father n) m))))))

;;; i.
;;; Use the functions you have written to answer the following questions:
;;; 1. Is Robert descended from Deirdre?
(descended-from 'robert 'deirdre)
NIL

;;; 2. Who are Yvette's ancestors?
(ancestors 'yvette)
(ZELDA ROBERT JULIE QUENTIN WANDA VINCENT)

;;; 3. What is the generation gap between Olivia and Frank?
(generation-gap 'olivia 'frank)
3

;;; 4. Who are Peter's cousins?
(cousins 'peter)
(JOSHUA ROBERT)

;;; 5. Who are Olivia's grandparents?
(grandparents #'parents 'olivia)
(GEORGE ELLEN HILLARY ANDRE)

;;; Ex 8.61
;;; Write a tail-recursive version of COUNT-UP.
(defun count-up (n)
  (tr-count-up n nil))

(defun tr-count-up (n result)
  (cond ((zerop n) result)
        (t (tr-count-up (- n 1) (cons n result)))))

;;; Ex 8.62
;;; Write a tail-recursive version of FACT.
(defun fact (n)
  (tr-fact (- n 1) (* (- n 1) n)))

(defun tr-fact (n result)
  (cond ((eq n 1) result)
        (t (tr-fact (- n 1) (* (- n 1) result)))))

;;; Write tail-recursive versions of UNION, INTERSECTION, and SET-DIFFERENCE. Your functions need not return results in the same order as the built-in functions.
;;; UNION
(defun my-union (a b)
  (tr-my-union a b nil))

(defun tr-my-union (a b result)
  (cond ((null a) (append b result))
        (t (tr-my-union (rest a) (remove (first a) b) 
                        (cons (first a) result)))))

;;; INTERSECTION
(defun my-intersection (a b)
  (tr-my-intersection a b nil))

(defun tr-my-intersection (a b result)
  (cond ((null a) result)
        (t (tr-my-intersection (rest a) b (if (member (first a) b)
                                              (cons (first a) result)
                                              result)))))

;;; SET-DIFFERENCE
(defun my-set-difference (a b)
  (tr-set-difference a b nil))

(defun tr-set-difference (a b result)
  (cond ((null a) result)
        (t (tr-set-difference (rest a) b (if (not (member (first a) b))
                                               (cons (first a) result)
                                               result)))))

;;; Ex 8.64
;;; Write a TREE-FIND-IF operator that returns the first non-NIL atom of a tree that satisfies a predicate.
;;; (TREE-FIND-IF #'ODDP '((2 4) (5 6) (7))) should return 5.
(defun tree-find-if (fn x)
  (cond ((null x) nil)
        (t (or (tr-tree-find-if fn (first x)) (tree-find-if fn (rest x))))))

(defun tr-tree-find-if (fn x)
  (cond ((null x) nil)
        ((funcall fn (first x)) (first x))
        (t (tr-tree-find-if fn (rest x)))))

;;; 8.65
;;; Use LABELS to write versions of TR-COUNT-SLICES and TR-REVERSE.

;;; TR-COUNT-SLICES
(defun tr-count-slices (loaf)
  (tr-cs1 loaf 0))

(defun tr-cs1 (loaf n)
  (cond ((null loaf) n)
        (t (tr-cs1 (rest loaf) (+ n 1)))))

(defun tr-count-slices (loaf)
  (labels ((tr-cs1 (loaf n)
             (cond ((null loaf) n)
                   (t (tr-cs1 (rest loaf) (+ n 1))))))
    (tr-cs1 loaf 0)))

;;; TR-REVERSE
(defun tr-reverse (x)
  (tr-rev1 x nil))

(defun tr-rev1 (x result)
  (cond ((null x) result)
        (t (tr-rev1
            (rest x)
            (cons (first x) result)))))

(defun tr-reverse (x)
  (labels ((tr-rev1 (x result)
             (cond ((null x) result)
                   (t (tr-rev1 (rest x) (cons (first x) result))))))
    (tr-rev1 x nil)))

;;; Ex 8.66
;;; Write ARITH-EVAL, a function that evaluates arithmetic expressions.
;;; (ARITH-EVAL '(2 + (3 * 4))) should return 14.
(defun arith-eval (x)
  (cond ((numberp x) x)
        (t (funcall (second x) (if (numberp (first x))
                                 (first x)
                                 (arith-eval (first x)))
                    (if (numberp (third x))
                        (third x)
                        (arith-eval (third x)))))))


(defun arith-eval (x)
  (cond ((numberp x) x)
        (t (funcall (second x)
                    (arith-eval (first x))
                    (arith-eval (third x))))))

;;; Ex 8.67
;;; Write a predicate LEGALP that returns T if its input is a legal arithmetic expression. For example, (LEGALP 4) and (LEGALP '((2 * 2) - 3)) should return T.
;;; (LEGALP NIL) and (LEGALP '(A B C D)) should return NIL.
(defun legalp (x)
  (cond ((null x) nil) 
        ((numberp x) t)
        ((or (symbolp (first x)) (symbolp (third x))) nil)
        (t (and (legalp (first x)) (symbolp (second x)) (legalp (third x))))))

;;; Alternative solution
(defun legalp (x)
  (cond ((numberp x) t)
        ((atom x) nil)
        (t (and (legalp (first x))
                (member (second x)
                        '(+ - * /))
                (legalp (third x))))))

;;; Ex 8.69
;;; Of the positive integers greater than one, some are primes while others are not. Primes are numbers that are divisible only by themselves and by 1.
;;; A nonprime, which is known as a composite number, can always be factored into primes. The number 60 has factors 2, 2, 3 and 5 which means 60 = 2x2x3x5.
;;; Write a recursive definition for positive integers greater than one in terms of prime numbers.
(defun factors (n)
  (factors-help n 2))

(defun factors-help (n p)
  (cond ((equal n 1) nil)
        ((zerop (rem n p))
         (cons p (factors-help (/ n p) p)))
        (t (factors-help n (+ p 1)))))

;;; Ex 8.70
;;; Write a FACTOR-TREE function that returns a factorization tree.
;;; (FACTOR-TREE 60) should return the list (60 2 (30 2 (15 3 5))).
(defun factor-tree (n)
  (factor-tree-help n 2))

(defun factor-tree-help (n p)
  (cond ((eq n p) n) 
        ((zerop (rem n p))
         (list n p (factor-tree-help (/ n p) p)))
        (t (factor-tree-help n (+ p 1)))))

