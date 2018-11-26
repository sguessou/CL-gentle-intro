;;; Chapter 6 - List Data Structures
;;; Exercises

;;; Ex 6.6
;;; Use the LAST function to write a function called LAST-ELEMENT that returns the last element of a list instead of the last cons cell.
;;; Write another version of LAST-ELEMENT using REVERSE instead of LAST. Write another version using NTH and LENGTH.
(defun last-element (l)
  "Returns last element of list version 1"
  (car (last l)))

(defun last-element (l)
  "Returns last element of list version 2"
  (car (reverse l)))

(defun last-element (l)
  "Returns last element of list version 3"
  (nth (- (length l) 1) l))

;;; Ex 6.7
;;; Use REVERSE to write a NEXT-TO-LAST function that returns the next-to-last element of a list. Write another version using NTH.
(defun next-to-last (l)
  (cadr (reverse l)))

(defun next-to-last (l)
  (nth (- (length l) 2) l)) 

;;; Ex 6.8
;;; Write a function MY-BUTLAST that returns a list with the last element removed.
;;; (MY-BUTLAST '(ROSES ARE RED)) should return the list (ROSES ARE). (MY-BUTLAST '(G A G A)) should return (G A G).
(defun my-butlast (l)
  (reverse (rest (reverse l))))

 
;;; Ex 6.9
;;; What primitive function does the following reduce to?
(defun mystery (x)
  (first (last (reverse x)))) 
;;; answer: CAR

;;; Ex 6.10
;;; A palindrome is a sequence that reads the same forward and backwards. The list (A B C D C B A) is a palindrome; (A B C A B C) is not.
;;; Write a function PALINDROMEP that returns T if its input is a palindrome.
(defun palindromep (l)
  (equal l (reverse l))) 

;;; Ex 6.11
;;; Write a function MAKE-PALINDROME that makes a palindrome out of a list, for example, given (YOU AND ME) as input it should return (YOU AND ME ME AND YOU).
(defun make-palindrome (l)
  (append l (reverse l)))

;;; Ex 6.15
;;; We can use MEMBER to write a predicate that returns a true value if a sentence contains the word "the."
(defun contains-the-p (sent)
  (member 'the sent))
;;; Suppose we instead want a predicate CONTAINS-ARTICLE-P that returns a true value if a sentence contains any article, such as "the," "a," or "an." Write a version of this predicate using INTERSECTION. 
;;; Write another version using MEMBER and OR. Could the problem be solved with AND instead or OR?
(defun contains-article-p (sent)
  (intersection '(the a an) sent))

(defun contains-article-p (sent)
  (or (member 'the sent) (member 'a sent) (member 'an sent)))

(defun contains-article-p (sent)
  (not (and  (not (member 'the sent)) 
             (not (member 'a sent))  
             (not (member 'an sent)))))

;;; Ex 6.18
;;; Write a function ADD-VOWELS that takes a set of letters as input and adds the vowels (A E I O U) to the set. 
;;; For example, calling ADD-VOWELS on the set (X A E Z) should produce the set (X A E Z I O U), except that the exact order of the elements in the result is unimportant.
(defun add-vowels (s)
  (union s '(a e i o u)))

;;; Ex 6.21
;;; If set x is a subset of set y, then subtracting y from x should leave the empty set.
;;; Write MY-SUBSETP, a version of the SUBSETP predicate that returns T if its first input is a subset of its second input.
(defun my-subsetp (a b)
  (not (set-difference a b)))

;;; Ex 6.24
;;; Sets are said to be equal if they contain exactly the same elements.
;;; Order does not matter in a set, so the sets (RED BLUE GREEN) and (GREEN BLUE RED) are considered equal.
;;; However, the EQUAL predicate does not consider them equal, because it treats them as lists, not as sets. Write a SET-EQUAL predicate that returns T if two things are equal as sets.
;;; (Hint: If two sets are equal, then each is a subset of the other.)
(defun set-equal (a b)
  (and (subsetp a b) (subsetp b a))) 

;;; Ex 6.25
;;; A set X is a proper subset of a set Y if X is a subset of Y but not equal to Y.
;;; Thus, (A C) is a proper subset of (C A B). (A B C) is a subset of (C A B), but not a proper subset of it.
;;; Write the PROPER-SUBSETP predicate, which returns T if its first input is a proper subset of its second input.
(defun proper-subset (a b)
  (and (subsetp a b) (not (subsetp b a)))) 

;;; Ex 6.26
;;; We are going to write a program that compares the descriptions of two objects and tells how many features they have in common.
;;; The descriptions will be represented as a list of features, with the symbol -VS- separating the first object from the second.
;;; Thus, when given a list like (large red shiny cube -vs- small shiny red four-sided pyramid)
;;; the program will respond with (2 COMMON FEATURES). We will compose this program from several small functions that you will write and test one at a time.
;;; a.
;;; Write a function RIGHT-SIDE that returns all the features to the right of the -VS- symbol. RIGHT-SIDE of the list shown above should return (SMALL SHINY RED FOUR-SIDED PYRAMID).
;;; Test your function to make sure it works correctly.
(defun right-side (o)
  (cdr (member '-vs- o)))

;;; b.
;;; Write a function LEFT-SIDE that returns all the features to the left of the -VS-. You can't use the MEMBER trick directly for this one.
(defun left-side (o)
  (cdr (member '-vs- (reverse o))))

;;; c.
;;; Write a function COUNT-COMMON that returns the number of features the left and right sides of the input have in common.
(defun count-common (o)
  (length (intersection (left-side o) (right-side o))))

;;; d.
;;; Write the main function, COMPARE, that takes a list of features describing two objects, with a -VS- between them, and reports the number of features they have in common.
;;; COMPARE should return a list of form (n COMMON FEATURES).
(defun compare (o)
  (cons (count-common o) '(common features)))
