;;; Chapter 5 - Variables and Side Effects
;; Exercises

;; Ex 5.1
; Rewrite function POOR-STYLE to create a new local variable Q using LET, instead of using SETF to change P.
; Call your new function GOOD-STYLE.
(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))
