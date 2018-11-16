;;; Chapter 4 - Conditionals
;; Exercises

;; Ex 4.1
; Write a function MAKE-EVEN that makes an odd number even by adding one to it. 
; If the input to MAKE-EVEN is already even, it should be returned unchanged.
(defun make-even (n)
  (if (evenp n)
      n
      (1+ n)))
