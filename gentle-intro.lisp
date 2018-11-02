;;;; Ex 1.15
;;; Write a predicate NOT-ONEP that return T if its input is anything other than one
(defun not-onep (n)
   (not (equal n 1)))


;;;; Ex 1.16
;;; Write the predicate NOT-PLUSP that returns T if its input is not greater than zero
(defun not-plusp (n)
  (not (> n 0)))
