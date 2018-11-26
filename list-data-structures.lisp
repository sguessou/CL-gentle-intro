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

