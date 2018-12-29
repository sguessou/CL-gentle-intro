;;; Chapter 9 - Input/Output
;;; Exercises.

;;; Ex 9.1
;;; Write a function to print the following saying on the display: "There are old pilots, and there are bold pilots, but there are no old bold pilots."
;;; The function should break up the quotation into several lines.
(defun print-test ()
  (format t "There are old pilots,~&and there are bold pilots,~&but there are no old bold pilots.~& "))

;;; Ex 9.2
;;; Write a recursive function DRAW-LINE that draws a line of a specified length by doing (FORMAT T "*") the correct number of times.
;;; (DRAW-LINE 10) should produce **********
(defun draw-line (n)
  (labels ((dl-helper (x result)
             (cond ((>= x n) (mapcar #'(lambda (x) (format t x)) result))
                   (t (dl-helper (+ x 1) (cons "*" result))))))
    (dl-helper 0 nil)))

(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
        (t (format t "*")
           (draw-line (- n 1)))))

;;; Ex 9.3
;;; Write a recursive function DRAW-BOX that calls DRAW-LINE repeatedly to draw a box of specified dimensions.
;;; (DRAW-BOX 10 4) should produce:
               
                **********
                **********
                **********
                **********
(defun draw-box (x y)
  (cond ((zerop y) (format t "~%"))
        (t (draw-line x)
           (format t "~%")
           (draw-box x (- y 1)))))

;;; Ex 9.4
;;; Write a recursive function NINETY-NINE-BOTTLES that sings the well-known song "Ninety-nine Bottles of Beer on the Wall." The first verse of this song is:
;;; 99 bottles of beer on the wall,
;;; 99 bottles of beer!
;;; Take one down,
;;; Pass it around,
;;; 98 bottles of beer on the wall.
;;; NINETY-NINE-BOTTLES should take a number N as input and start counting from N down to zero. (This is so you can run it on three bottles instead of all ninety nine.)
;;; Your function should also leave a blank line between each verse, and say something appropriate when it runs out of beer.
(defun ninety-nine-bottles (n)
  (cond ((zerop n) (format t "~%~S bottles of beer on the wall." n))
        (t (format t "~&~S bottles of beer on the wall," n)
           (format t "~&~S bottles of beer!" n)
           (format t "~&Take one down,")
           (format t "~&Pass it around,")
           (ninety-nine-bottles (- n 1)))))

;;; Ex 9.5
;;; Part of any tic-tac-toe playing program is a function to display the board. Write a function PRINT-BOARD that takes a list of nine element as input. Each element will be an X, an O, or NIL. PRINT-BOARD should display the corresponding board.
;;; (PRINT-BOARD '(X O O NIL X NIL O NIL X)) should print:
              X | O | O
             -----------
                | X |   
             -----------
              O |   | X

(defun repl (a b c)
  (cond ((eq a b) c)
        (t a)))

(defun print-board (l)
  (cond ((null l) (format t "~%"))
        (t (format t "~& ~A | ~A | ~A" 
                   (repl (first l) nil " ")
                   (repl (second l) nil " ")
                   (repl (third l) nil " "))
           (if (cdddr l)
               (format t "~&-----------"))
           (print-board (cdddr l)))))

(defun print-board (b)
  (let ((b2 (sublis '((x   . "X")
                      (o   . "O")
                      (nil . " "))
                    b)))
    (format t "~&")
    (print-line b2)
    (format t "-----------~%")
    (print-line (nthcdr 3 b2))
    (format t "-----------~%")
    (print-line (nthcdr 6 b2))))

(defun print-line (line)
  (format t " ~A | ~A | ~A~%"
          (first line)
          (second line)
          (third line)))

;;; Ex 9.6
;;; Write a function to compute an hourly worker's gross pay given an hourly wage in dollars and the number of hours he or she worked.
;;; Your function should prompt for each input it needs by printing a message in English. It should display its answers in English as well.
(defun gross-pay ()
  (format t "~&Please enter your hourly wage: ")
  (let ((h (read)))
    (format t "~&Please enter the working hours: ")
    (let ((w (read)))
      (format t "~&The gross pay is ~S €." (* h w)))))

;;; Ex 9.7
;;; The COOKIE-MONSTER function keeps reading data from the terminal until it reads the symbol COOKIE.
;;; Write COOKIE-MONSTER. Here is a sample interaction:
> (cookie-monster)
Give me cookie!!!
Cookie? rock
No want ROCK...

Give me cookie!!!
Cookie? cookie
Thank you!...Munch munch munch...BURP
NIL

(defun cookie-monster ()
  (format t "~&Give me cookie!!!")
  (format t "~&Cookie? ")
  (let ((cookie (read)))
    (cond ((string-equal cookie "cookie") 
           (format t "~&Thank you!...Munch munch munch...BURP"))
          (t (format t "~&No want ~A..." cookie) 
             (cookie-monster)))))

;;; Ex 9.10
;;; As you write each of the following functions, test it by calling it from top level with appropriate inputs before proceeding on to the next function.
;;; a.
;;; Write a recursive function SPACE-OVER that takes a number N as input and moves the cursor to the right by printing N spaces, one at a time.
;;; SPACE should print "Error!" if N is negative.
;;; Test it by using the function TEST. Try (TEST 5) and (TEST -5).
(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(defun space-over (n)
  (cond ((zerop n) nil)
        ((< n 0) (format t "Error!"))
        (t (format t " ")
           (space-over (- n 1)))))

;;; b.
;;; Write a function PLOT-ONE-POINT that takes two inputs PLOTTING-STRING and Y-VAL, prints PLOTTING-STRING (without the quotes) in column Y-VAL, and then moves to a new line. 
;;; The leftmost column is numbered zero.
(defun plot-one-point ()
  (format t "~&Enter plotting string: ")
  (let ((plotting-string (read)))
    (format t "~&Enter y-val: ")
    (let ((y-val (read)))
      (space-over y-val)
      (format t "~A" plotting-string))))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))

;;; c.
;;; Write a function PLOT-POINTS that takes a string and a list of y values as input and plot them. (PLOT-POINTS "<>" '(4 6 8 10 8 6 4)) should print
   <>
     <>
       <>
         <>
       <>
     <>
   <>

(defun plot-points (str l)
  (mapcar #'(lambda (x) (plot-one-point str x)) l))

;;; d.
;;; Write a function GENERATE that takes two numbers M and N as input and returns a list of the integers from M to N.
;;; (GENERATE -3 3) should return (-3 -2 -1 0 1 2 3).
(defun generate (m n)
  (generate-helper m n nil))

(defun generate-helper (m n result)
  (cond ((> m n) result)
        (t (generate-helper (+ m 1) n (append result (list m))))))

;;; e.
;;; Write the MAKE-GRAPH function. MAKE-GRAPH should prompt for the values of FUNC, StART, END, and PLOTTING-STRING, and then graph the function.
;;; Note: You can pass FUNC as an input to MAPCAR to generate the list of y values for the function.
(defun make-graph ()
  (format t "~&Function to graph?     ")
  (let ((func (read)))
    (format t "~&Starting x value?    ")
    (let ((start (read)))
      (format t "~&Ending x value?    ")
      (let ((end (read)))
        (format t "~&Plotting string? ")
        (let ((plotting-string (read)))
          (plot-points plotting-string 
                       (mapcar #'(lambda (x) (funcall func x)) (generate start end))))))))  

;;; f.
;;; Define the SQUARE function and graph it over the range -7 to 7.
(defun square (x)
  (* x x))

;;; Ex 9.11
;;; Write a function DOT-PRIN1 that takes a list as input and prints it in dot notation. DOT-PRIN1 will print parentheses by (FORMAT T "(") and (FORMAT T ")"), and dots by (FORMAT T " . "), and will call itself recursively to print lists within lists.
;;; DOT-PRIN1 should return NIL as its result. 
;;; Try (DOT-PRIN1 '(A (B) C)) and see if your output matches the result in the table above.
;;; Then try (DOT-PRIN1 '((((A))))).
(defun dot-prin1 (l)
  (cond ((atom l) (format t "~S" l))
        (t (format t "(")
           (dot-prin1 (car l))
           (format t " . ")
           (dot-prin1 (cdr l))
           (format t ")"))))

