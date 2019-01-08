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

;;; Ex 11.4
;;; Write an iterative version of LENGTH, called IT-LENGTH.
(defun it-length (x)
  (let ((c 0))
    (dolist (element x c)
      (setf c (+ c 1)))
    c))

;;; Ex 11.5
;;; Write an iterative version of NTH, called IT-NTH.
(defun it-nth (i x)
  (let ((c 0))
    (dolist (element x c)
      (if (or (< i 0) (>= i (length x))) (return nil))
      (if (equal c i)
          (return element))
      (setf c (+ c 1)))))

;;; Ex 11.6
;;; Write an iterative version of UNION, called IT-UNION. Your function need not return its result in the same order as the built-in UNION function.
(defun it-union (a b)
  (let ((l b))
    (dolist (element a)
    (if (not (member element l))
        (push element l)))
    l))
