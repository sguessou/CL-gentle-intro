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
