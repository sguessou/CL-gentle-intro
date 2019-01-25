;;; Chapter 14 - Macros and Compilation

;;; Exercises

;;; Ex 14.3
;;; Write a SET-NIL macro that sets a variable to NIL.
(defmacro set-nil (var)
  (list 'setq var nil))

;;; Ex 14.4
;;; Write a macro called SIMPLE-ROTATEF that switches the value of two variables.
;;; for example, if A is two and B is seven, then (SIMPLE-ROTATEF A B) shoul make A seven and B two.
;;; Obviously, setting A to B first, and then setting B to A won't work.
;;; Your macro should expand into a LET expression that holds on to the original values of the two variables and then assigns them their new values in its body.
(defmacro simple-rotatef (a b)
  `(let ((clone-a ,a)
         (clone-b ,b))
    (setf ,a clone-b)
    (setf ,b clone-a))) 

;;; Ex 14.5
;;; Write a macro SET-MUTUAL that takes two variable names as input and expands into an expression that sets each variable to the name of the other. 
;;; (SET-MUTUAL A B) should set A to 'B, and B to 'A.
(defmacro set-mutual (a b)
  `(let ((var-a ',b)
         (var-b ',a))
     (setf ,a var-a)
     (setf ,b var-b)))

;;; Ex 14.6
;;; Write a macro called VARIABLE-CHAIN that accepts any number of inputs.
;;; The expression (VARIABLE-CHAIN A B C D) should expand into an expression that sets A to 'B, B to 'C, and C to 'D.
(defmacro set-zero (&rest vars)
  `(progn ,@(mapcar #'(lambda (var)
                        (list 'setf var 0))
                    vars)
          '(zeroed ,@vars)))

(defmacro variable-chain (&rest vars)
  `(progn ,@(mapcar #'(lambda (a b)
              `(setf ,a ',b))
          vars
          (rest vars))
          '(done ,@vars)))

