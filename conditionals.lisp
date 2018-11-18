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


