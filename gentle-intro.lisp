;;;; Ex 1.15
;;; Write a predicate NOT-ONEP that return T if its input is anything other than one
(defun not-onep (n)
   (not (equal n 1)))


;;;; Ex 1.16
;;; Write the predicate NOT-PLUSP that returns T if its input is not greater than zero
(defun not-plusp (n)
  (not (> n 0)))

;;;; Ex 1.17
;;; Some earlier Lisp dialects did not have the EVENP primitive; they only had ODDP. Show how to define EVENP in terms of ODDP.
(defun my-evenp (n)
  (not (oddp n)))

;;;; Ex 1.20
;;; Write XOR, the exclusive-or truth function.
(defun xor (n m)
  (not (eq (not n) (not m))))
