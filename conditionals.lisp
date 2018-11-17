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
