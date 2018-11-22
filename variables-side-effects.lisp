;;; Chapter 5 - Variables and Side Effects
;;; Exercises

;;; Ex 5.1
;;; Rewrite function POOR-STYLE to create a new local variable Q using LET, instead of using SETF to change P.
;;; Call your new function GOOD-STYLE.
(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))

;;; Ex 5.6 a
;;; Write a function THROW-DIE that returns a random number from 1 to 6, inclusive. Remember that (RANDOM 6) will pick numbers from 0 to 5.
;;; THROW-DIE doesn't need any inputs, so its argument list should be NIL.
(defun throw-die ()
  "Returns a random number from 1 to 6."
  (+ (random 6) 1))

