;;; Chapter 4 - Conditionals
;; Exercises

;; Ex 4.1
; Write a function MAKE-EVEN that makes an odd number even by adding one to it. 
; If the input to MAKE-EVEN is already even, it should be returned unchanged.
(defun make-even (n)
  (if (evenp n)
      n
      (1+ n)))

;; Ex 4.2 
; Write a function FURTHER that makes a positive number larger by adding one to it, and a negative number smaller by subtracting one from it.
; What does the function do if given the number 0.
(defun further (n)
  (if (> n 0)
      (1+ n)
      (1- n)))

;; Ex 4.3
; Recall the primitive function NOT: It returns NIL for a true input and T for a false one. Suppose Lisp didn't have a NOT primitive.
; Show how to write NOT using just IF and constants (no other functions). Call the function MY-NOT
(defun my-not (in)
  (if (and in t)
      t
      nil))

;; Ex 4.4
; Write a function ORDERED that takes two numbers as input and makes a list of them in ascending order. 
; (ORDERED 4 3) should also return (3 4), in other words, the first and second inputs should appear in reverse order when the first is greater than the second.
(defun ordered (a b)
  (if (> a b)
      (list b a)
      (list a b)))

;; Ex 4.6
; Write a version of the absolute value function MY-ABS using COND instead of IF.
(defun my-abs (n)
  (cond ((< n 0) (* n -1))
        (t n)))

;; Ex 4.8
; Write EMPHASIZE3, which is like EMPHASIZE2 but adds the symbol VERY onto the list if it doesn't know how to emphasize it.
; For example, EMPHASIZE3 of (LONG DAY) should produce (VERY LONG DAY).
; What does EMPHASIZE3 of (VERY LONG DAY) produce?
(defun emphasize2 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad (cons 'awful (rest x))))
        (t x)))

(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))))

;; Ex 4.9
; What is wrong with this function? Try out the function on the numbers 3, 4 and -2.
; Rewrite it so it works correctly.
(defun make-odd-w (x)
  (cond (t x)
        ((not (oddp x)) (+ x 1))))

(defun make-odd-c (x)
  (cond ((oddp x) (+ x 1))
        (t x)))


;; Ex 4.10
; Write a function CONSTRAIN that takes three inputs called X, MAX, and MIN. If X is less than MIN, it should return MIN; if X is greater than MAX, it should return MAX.
; Otherwise, since X is between MIN and MAX, it should return X. (CONSTRAIN 3 -50 50) should return 3. (CONSTRAIN 92 -50 50) should return 50.
; Write one version using COND and another using nested IFs.
(defun constrain-v1 (x max min)
  (cond ((> max x) max)
        ((< min x) min)
        (t x)))

(defun constrain-v2 (x max min)
  (if (> max x)
      max)
  (if (< min x)
      min)
  x)

;; Ex 4.11
; Write a function FIRSTZERO that takes a list of three numbers as input and returns a word (one of "first," "second," "third," or "none") indicating where the first zero appears in the list.
; Example: (FIRSTZERO '(3 0 4)) should return SECOND. What happens if you try to call FIRSTZERO with three separate numbers instead of a list of three numbers, as in (FIRSTZERO 3 0 4)?
(defun firstzero (l)
  (cond ((equal (first l) 0) 'first)
        ((equal (second l) 0) 'second)
        ((equal (third l) 0) 'third)
        (t 'none)))

;; Ex 4.12
; Write a function CYCLE that cyclically counts from 1 to 99 . CYCLE called with an input of 1 should return 2, with an input of 2 should return 3, and so on.
; With an input of 99, CYCLE should return 1. That's the cyclical part. Do not try to solve this with 99 COND clauses!
(defun cycle (n)
  (cond ((>= n 99) 1)
        (t (1+ n))))

;; Ex 4.13
; Write a function HOWCOMPUTE that is the inverse of the COMPUTE function described previously.
; HOWCOMPUTE takes three numbers as inputs and figures out what operation would produce the third from the first two.
; (HOWCOMPUTE 3 4 7) should return SUM-OF.
; (HOWCOMPUTE 3 4 12) should return PRODUCT-OF.
; HOWCOMPUTE should return the list BEATS-ME if it can't find a relationship between the firs two inputs and the third.
(defun howcompute (a b c)
  (cond ((eq (+ a b) c) 'sum-of)
        ((eq (* a b) c) 'product-of)
        (t '(beats me))))

;; Ex 4.15 
; Write a predicate called GEQ that returns T if its first input is greater than or equal to its second input.
(defun geq (a b)
  (cond ((>= a b) t)))

;; Ex 4.16
; Write a function that squares a number if it is odd and positive, doubles it if it is odd and negative, and otherwise divides the number by 2.
(defun fn (n)
  (cond ((and (> n 0) (oddp n)) (* n n))
        ((and (< n 0) (oddp n)) (* n 2))
        (t (/ n 2))))

;; Ex 4.17
; Write a predicate that returns T if the first input is either BOY or GIRL and the second input is CHILD, or the first input is either MAN or WOMAN and the second input is ADULT.
(defun fn (a b)
  (cond ((and (eq a 'boy) (eq b 'child)) t)
        ((and (eq a 'girl) (eq b 'child)) t)
        ((and (eq a 'man) (eq b 'adult)) t)
        ((and (eq a 'woman) (eq b 'adult)) t)))

;; Ex 4.18
; Write a function to act as referee in the Rock-Scissors-Paper game. In this game, each player picks one of Rock, Scissors, or Paper, and then both players tell what they picked.
; Rock "breaks" Scissors, so if the first player picks Rock and the second picks Scissors, the first player wins.
; Scissors "cuts" Paper, and Paper "covers" Rock. If both players pick the same thing, it's a tie.
; The function PLAY should take two inputs, each of which is either ROCK, SCISSORS, or PAPER, and return one of the symbols FIRST-WINS, SECOND-WINS, or TIE.
; Examples: (PLAY 'ROCK 'SCISSORS) should return FIRST-WINS.
; (PLAY 'PAPER 'SCISSORS) should return SECOND-WINS.
(defun play (a b)
  (cond ((or (and (eq a 'paper) (eq b 'rock))
             (and (eq a 'rock) (eq b 'scissors))
             (and (eq a 'scissors) (eq b 'paper))) 'first-wins)
        ((or (and (eq a 'scissors) (eq b 'rock))
             (and (eq a 'paper) (eq b 'scissors))
             (and (eq a 'rock) (eq b 'paper))) 'second-wins)
        ((eq a b) 'tie)))

;; Ex 4.22
; Use COND to write a predicate BOILINGP that takes two inputs, TEMP and SCALE, and returns T if the temperature is above the boiling point of water on the specified scale.
; If the scale is FAHRENHEIT, the boiling point is 212 degrees; if CELSIUS, the boiling point is 100 degrees. Also write versions using IF and AND/OR instead of COND.
(defun boilingp (temp scale)
  (cond ((or (and (eq scale 'fahrenheit) (> temp 212))
            (and (eq scale 'celsius) (> temp 100))) t)))

(defun boilingp (temp scale)
  (if (or (and (eq scale 'fahrenheit) (> temp 212))
         (and (eq scale 'celsius) (> temp 100)))
      t))

;; Ex 4.29
; Write versions of LOGICAL-AND using IF and COND instead of AND.
(defun logical-and (x y)
  (and x y t))

(defun logical-and (x y)
  (cond ((not x) nil)
        ((not y) nil)
        (t t)))

(defun logical-and (x y)
  (if (not x) nil
      (if (not y) nil
          t)))

;; Ex 4.30
; Write LOGICAL-OR. Make sure it returns only T or NIL for its result.
(defun logical-or (x y)
  (not (not (or x y))))
