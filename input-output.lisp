;;; Chapter 9 - Input/Output
;;; Exercises.

;;; Ex 9.1
;;; Write a function to print the following saying on the display: "There are old pilots, and there are bold pilots, but there are no old bold pilots."
;;; The function should break up the quotation into several lines.
(defun print-test ()
  (format t "There are old pilots,~&and there are bold pilots,~&but there are no old bold pilots.~& "))
