;;; Chapter 9 - Input/Output
;;; Exercises.

;;; Ex 9.1
;;; Write a function to print the following saying on the display: "There are old pilots, and there are bold pilots, but there are no old bold pilots."
;;; The function should break up the quotation into several lines.
(defun print-test ()
  (format t "There are old pilots,~&and there are bold pilots,~&but there are no old bold pilots.~& "))

;;; Ex 9.2
;;; Write a recursive function DRAW-LINE that draws a line of a specified length by doing (FORMAT T "*") the correct number of times.
;;; (DRAW-LINE 10) should produce **********
(defun draw-line (n)
  (labels ((dl-helper (x result)
             (cond ((>= x n) (mapcar #'(lambda (x) (format t x)) result))
                   (t (dl-helper (+ x 1) (cons "*" result))))))
    (dl-helper 0 nil)))

(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
        (t (format t "*")
           (draw-line (- n 1)))))

;;; Ex 9.3
;;; Write a recursive function DRAW-BOX that calls DRAW-LINE repeatedly to draw a box of specified dimensions.
;;; (DRAW-BOX 10 4) should produce:
               
                **********
                **********
                **********
                **********
(defun draw-box (x y)
  (cond ((zerop y) (format t "~%"))
        (t (draw-line x)
           (format t "~%")
           (draw-box x (- y 1)))))
