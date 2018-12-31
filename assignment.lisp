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

;;; Ex 10.3
;;; Modify the MEET function to keep a count of how many people have been met more than once. Store this count in a global variable.

(setf *friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*))
         'we-just-met)
        ((member person *friends*)
         'we-know-each-other)
        (t (push person *friends*)
           'please-to-meet-you)))

(setf *meet-cnt* 0)

(defun meet (person)
  (cond ((equal person (first *friends*))
         'we-just-met)
        ((member person *friends*)
         (+ *meet-cnt* 1)
         'we-know-each-other)
        (t (push person *friends*)
           'please-to-meet-you)))

;;; Ex 10.4 
;;; Write a function FORGET that removes a person from the *FRIENDS* list.
;;; If the person wasn't on the list in the first place, the function should complain.
(defun forget (person)
  (cond ((member person *friends*) (remove person *friends*))
        (t (format t "~&You haven't met ~S yet!" person))))

