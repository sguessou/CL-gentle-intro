;;; Chapter 11 - Iteration and Block Structure
;;; Exercises

;;; Ex 11.1
;;; Write an iterative version of the MEMBER function, called IT-MEMBER. It should return T if its first input appears in its second input;
;;; It needs not return a sublist of its second input.
(defun it-member (item b)
  (dolist (e b)
    (when (equal item e)
        (return t))))

;;; Ex 11.2
;;; Write an iterative version of ASSOC, called IT-ASSOC.
(defun it-assoc (item x)
  (dolist (e x)
    (when (equal (car e) item)
      (return e))))

;;; Ex 11.3
;;; Write a recursive version of CHECK-ALL-ODD. It should produce the same message and the same result as the preceding iterative version.
;;; Iterative version
(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))

;;; Recursive version
(defun check-all-odd (x)
  (cond ((null x) t)
        ((not (oddp (car x))) nil)
        (t (format t "~&Checking ~S..." (car x)) 
           (check-all-odd (rest x)))))

;;; Recursive version with unless
(defun check-all-odd (x)
  (cond ((null x) t)
        (t (format t "~&Checking ~S..."
                   (first x))
           (unless (evenp (first x))
             (check-all-odd (rest x))))))
