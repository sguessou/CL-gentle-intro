;;; Chapter 14 - Macros and Compilation

;;; Exercises

;;; Ex 14.3
;;; Write a SET-NIL macro that sets a variable to NIL.
(defmacro set-nil (var)
  (list 'setq var nil))
