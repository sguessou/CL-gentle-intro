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

;;; Ex 5.6 c
;;; Throwing two ones is called "snake eyes"; two sixes is called "boxcars." Write predicates SNAKE-EYES-P and BOXCARS-P that takes a throw as input and return T if the throw is equal to (1 1) or (6 6), respectively.
(defun snake-eyes-p (l)
   (eq (+ (car l) (cadr l)) 2))

(defun boxcars-p (l)
   (eq (+ (car l) (cadr l)) 12))

;;; Ex 5.6 d
;;; In playing craps, the first throw of the dice is crucial. A throw of 7 or 11 is an instant win. A throw of 2, 3 or 12 is an instant loss (American casino rules).
;;; Write predicates INSTANT-WIN-P and INSTANT-LOSS-P to detect these conditions. Each should take a throw as input.
(defun instant-win-p (l)
  (let ((s (+ (car l) (cadr l))))
    (if (or (eq s 7) (eq s 11)) t))) 

(defun instant-loss-p (l)
  (let ((s (+ (car l) (cadr l))))
    (if (or (eq s 2) (eq s 3) (eq s 12)) t))) 

;;; Ex 5.6 e
;;; Write a function SAY-THROW that takes a throw as input and returns either the sum of the two dice or the symbol SNAKE-EYES or BOXCARS if the sum is 2 or 12.
;;; (SAY-THROW '(3 4)) should return 7. (SAY-THROW '(6 6)) should return BOXCARS.
(defun say-throw (l)
  (cond ((snake-eyes-p l) 'snake-eyes)
        ((boxcars-p l) 'boxcars)
        (t (+ (car l) (cadr l)))))

;;; Ex 5.6 f
;;; If you don't win or lose on the first throw of the dice, the value you threw becomes your "point," which will be explained shortly.
;;; Write a function (CRAPS) that produces the following sort of behaviour. Your solution should make use of the functions you wrote in previous steps.
;;; > (craps)
;;; (THROW 1 AND 1 -- SNAKEYES -- YOU LOSE)
;;; > (craps)
;;; (THROW 3 AND 4 -- 7 -- YOU WIN)
;;; > (craps)
;;; (THROW 2 AND 4 -- YOUR POINT IS 6)
(defun craps ()
  (let ((l (list (throw-die) (throw-die))))
    (cond ((instant-win-p l) 
           (list 'throw (car l) 'and (cadr l) '--
             (+ (car l) (cadr l)) 'you 'win))
          ((instant-loss-p l)
           (cond ((snake-eyes-p l) '(throw 1 and 1 -- snakeeyes -- you lose))
                 ((boxcars-p l) '(throw 6 and 6 -- boxcars -- you lose))
                 (t (list 'throw (car l) 'and (cadr l) '--
                          (+ (car l) (cadr l)) 'you 'lose))))
          (t (list 'throw (car l) 'and (cadr l) '--
              'your 'point 'is (+ (car l) (cadr l)))))))

;;; Ex 5.6 g
;;; Once a point has been established, you continue throwing the dice until you either win by making the point again or lose by throwing a 7.
;;; Write the function TRY-FOR-POINT that simulates this part of the game, as follows:
;;; > (try-for-point 6)
;;; (THROW 3 and 5 -- 8 -- THROW AGAIN) 
;;; > (try-for-point 6)
;;; (THROW 5 and 1 -- 6 -- YOU WIN)
;;; > (craps)
;;; (THROW 3 and 6 -- 9 -- YOUR POINT IS 9)
;;; > (try-for-point 9)
;;; (THROW 6 and 1 -- 7 -- YOU LOSE)
(defun try-for-point (x)
  (let* ((l (list (throw-die) (throw-die)))
         (s (+ (car l) (cadr l))))
    (if (eq s x) 
        (list 'throw (car l) 'and (cadr l) '-- s '-- 'you 'win)
        (if (eq (random 2) 1)
            (list 'throw (car l) 'and (cadr l) '-- s '-- 'throw 'again)
            (list 'throw (car l) 'and (cadr l) '-- s '-- 'you 'lose))))) 
