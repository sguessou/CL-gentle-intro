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
      (incf c))))

;;; Ex 11.5
;;; Write an iterative version of NTH, called IT-NTH.
(defun it-nth (i x)
  (let ((c 0))
    (dolist (element x c)
      (if (or (< i 0) (>= i (length x))) (return nil))
      (if (equal c i)
          (return element))
      (incf c))))

;;; alternative solution
(defun it-nth (n x)
  (dotimes (i n (first x))
      (pop x)))

;;; Ex 11.6
;;; Write an iterative version of UNION, called IT-UNION. Your function need not return its result in the same order as the built-in UNION function.
(defun it-union (a b)
  (let ((l b))
    (dolist (element a)
    (if (not (member element l))
        (push element l)))
    l))

;;; alternative solution
(defun it-union (x y)
  (dolist (e x y)
    (unless (member e y)
      (push e y))))

;;; Ex 11.8
;;; Write an iterative version of REVERSE, called IT-REVERSE.
(defun it-reverse (x)
  (let ((l '()))
     (dolist (element x l)
       (push element l))))

;;; Ex 11.9
;;; Show how to write CHECK-ALL-ODD using DO.
(defun check-all-odd (x)
  (do ((my-x x (rest my-x)))
      ((null my-x) (return t))
    (if (not (oddp (first my-x))) (return nil))
    (format t "~&Checking ~S..." (first my-x))))

;;; Ex 11.10
;;; Show how to write LAUNCH using DOTIMES.
(defun launch (n)
  (dotimes (i n (format t "Blast off!"))
     (format t "~S..." (- n i))))

;;; 11.11
;;; Rewrite the following function to use DO* instead of DOLIST.
(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (element (rest list-of-numbers) largest)
      (when (> element largest)
        (setf largest element)))))

(defun find-largest (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (y (first x) (first x))
        (largest y))
       ((null x) largest)
    (when (> y largest)
    (setf largest y))))

;;; 11.12
;;; Rewrite the following function to use DO instead of DOTIMES.
(defun power-of-2 (n)
  (let ((result 1))
    (dotimes (i n result)
      (incf result result)))) 

(defun power-of-2 (n)
  (do ((x 0 (+ x 1))
       (result 1))
      ((equal x n) result)
    (setf result (incf result result))))

;;; alternative solution
(defun power-of-2 (n)
  (do ((result 1 (+ result result))
       (i 0 (+ i 1)))
      ((equal i n) result)))

;;; Ex 11.13
;;; Rewrite the following function using DOLIST instead of DO*.
(defun first-non-integer (x)
  "Return the first non-integer element of X."
  (do* ((z x (rest z))
        (z1 (first z) (first z)))
       ((null z) 'none)
    (unless (integerp z1)
      (return z1))))

(defun first-non-integer (x)
  (dolist (e x 'none)
      (unless (integerp e)
        (return e))))

;;; alternative version
(defun first-non-integer (x)
  (dolist (e x 'none)
    (when (not (integerp e))
      (return e))))

;;; Ex 11.15
;;; corrected version
(defun ffo-with-do (x)
  (do* ((z x (rest z))
       (e (first z) (first z)))
      ((null z) nil)
    (if (oddp e) (return e))))

;;; Ex 11.18
;;; Rewrite the DOTIMES expression using DO.
(defun f ()
 (dotimes (i 5 i)
        (format t "~&I = ~S" i)))

(defun f ()
  (do ((i 0 (+ i 1)))
      ((equal i 5) i)
    (format t "~&I = ~S" i)))

;;; Ex 11.21
;;; One way to compute Fib(5) is to start with Fib(0) and Fib(1), which we know to be one, and add them together, giving Fib(2).
;;; Then add Fib(1) and Fib(2) to get Fib(3). Add Fib(2) and Fib(3) to get Fib(4).
;;; Add Fib(3) and Fib(4) to get Fib(5).
;;; This is an iterative method involving no recursion; we merely have to keep around the last two values of Fib to compute the next one.
;;; Write an iterative version of FIB using this technique.
(defun fib (n)
  (cond ((or (zerop n) (equal n 1)) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun fib (n)
  (do* ((i n (- i 1))   
        (a 0 b) 
        (b 1 c)
        (c 1 (+ a b)))
      ((zerop i) a)))
