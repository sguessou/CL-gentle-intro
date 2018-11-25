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
