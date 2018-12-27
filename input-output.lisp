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

;;; Ex 9.4
;;; Write a recursive function NINETY-NINE-BOTTLES that sings the well-known song "Ninety-nine Bottles of Beer on the Wall." The first verse of this song is:
;;; 99 bottles of beer on the wall,
;;; 99 bottles of beer!
;;; Take one down,
;;; Pass it around,
;;; 98 bottles of beer on the wall.
;;; NINETY-NINE-BOTTLES should take a number N as input and start counting from N down to zero. (This is so you can run it on three bottles instead of all ninety nine.)
;;; Your function should also leave a blank line between each verse, and say something appropriate when it runs out of beer.
(defun ninety-nine-bottles (n)
  (cond ((zerop n) (format t "~%~S bottles of beer on the wall." n))
        (t (format t "~&~S bottles of beer on the wall," n)
           (format t "~&~S bottles of beer!" n)
           (format t "~&Take one down,")
           (format t "~&Pass it around,")
           (ninety-nine-bottles (- n 1)))))
