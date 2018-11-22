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

;;; Ex 5.6 b
;;; Write a function THROW-DICE that throws two dice and returns a list of two numbers: the value of the first die and the value of the second. We'll call this list a "throw."
;;; For example, (THROW-DICE) migh return the throw (3 5), indicating that the first die was a 3 and the second a 5.
(defun throw-dice ()
  "Throws two dice and returns the result in a list"
  (list (throw-die) (throw-die)))

