;;; Chapter 8 - Recursion
;;; Exercises

;;; Ex 8.2
;;; Show how to write ANYODDP using IF instead of COND
(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

(defun anyoddp (x)
  (if (null x)
      nil
      (if (oddp (first x))
          t
          (anyoddp (rest x)))))
