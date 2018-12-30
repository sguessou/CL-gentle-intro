;;; Chapter 10 - Assignment
;;; Exercises

;;; Assign global variable *total-glasses* to zero.
(setf *total-glasses* 0)

(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (+ *total-glasses* n))
  (format t 
          "~&That makes ~S glasses so far today."
          *total-glasses*))

;;; Ex 10.2
;;; Rewrite the lemonade stand SELL function to use INCF instead of SETF.
(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (incf *total-glasses* n)
  (format t 
          "~&That makes ~S glasses so far today."
          *total-glasses*))
