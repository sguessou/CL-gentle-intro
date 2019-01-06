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

;;; CASE STUDY: a tic-tac-toe player
(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
   (format t "~&  ~A | ~A | ~A"
           (convert-to-letter x)
           (convert-to-letter y)
           (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(setf b (make-board))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *computer* 10)

(setf *opponent* 1)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
        (1 4 7) (2 5 8) (3 6 9)
        (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~& You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
                   "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move new-board)))))

(defun choose-best-move (board)
  "First version"
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board 
                           (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))
(defun block-opponent-win (board)
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board
                                          trip)
                             target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

;;; Ex 10.8
;;; a.
;;; Set up a global variable named *CORNERS* to hold a list of the four corner positions. Set up a global variable named *SIDES* to hold a list of the four side squares.
;;; Note that (FIND-EMPTY-POSITION BOARD *SIDES*) will return an empty side square, if there are any.
(setf *corners* '(1 3 7 9))

(setf *sides* '(2 4 6 8))

(setf *diagonals* '((1 5 9) (3 5 7)))

;;; b.
;;; Write a function BLOCK-SQUEEZE-PLAY that checks the diagonals for an O-X-O pattern and defends by suggesting a side square as the best move.
;;; Your function should return NIL if there is no squeeze play in progress.
;;; Otherwise, it should return a list containing a move number and a string explaining the strategy behind the move.
;;; Test the function by calling it on a sample board.
(defun block-squeeze-play (board)
  (let ((pos (block-squeeze-suggest-play board
              (+ (* 2 *opponent*) *computer*))))
    pos))

(defun block-squeeze-suggest-play (board target-sum)
  (let ((corner (find-if
                 #'(lambda (diagonal)
                      (equal (sum-diagonal board diagonal)
                             target-sum))
                  *diagonals*)))
    (when corner
      (let ((pos (find-empty-side board)))
        (and pos (list pos "block squeeze play"))))))

(defun find-empty-side (board)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           *sides*))

;;; c.
;;; Write a function BLOCK-TWO-ON-ONE that checks the diagonals for an O-O-X or X-O-O pattern and defends by suggesting a corner as the best move. 
;;; Your function should return NIL if there is no two-on-one threat to which to respond. Otherwise, it should return a list containing a move and a strategy description.
(defun block-two-on-one (board)
 (let ((pos (block-two-on-one-play board
				   (+ (* 2 *opponent*) *computer*))))
       pos))

(defun block-two-on-one-play (board target-sum)
  (let ((diagonal (find-if
                 #'(lambda (diagonal)
                     (equal (sum-diagonal board diagonal)
                            target-sum))
                 *diagonals*)))
    (when (diagonal
           (let ((pos (find-empty-corner board)))
             (and pos (list pos "block two on one play")))))))

(defun sum-diagonal (board diagonal)
  (+ (nth (first diagonal) board)
     (nth (second diagonal) board)
     (nth (third diagonal) board)))

(defun find-empty-corner (board)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           *corners*))

;;; Alternative solution for block-squeeze-play and block-two-on-one.
(defun block-squeeze-play (board)
  (sq-and-2 board *computer* *sides* 12
            "block squeeze play"))

(defun sq-and-2 (board player pool v strategy)
  (when (equal (nth 5 board) player)
    (or (sq-helper board 1 9 v strategy pool)
        (sq-helper board 3 7 v strategy pool))))

(defun sq-helper (board c1 c2 val strategy pool)
  (when (equal val (sum-triplet
                    board
                    (list c1 5 c2)))
    (let ((pos (find-empty-position
                board
                (or pool (list c1 c2)))))
      (and pos (list pos strategy)))))

(defun block-two-on-one (board)
   (sq-and-2 board *opponent* *corners* 12
             "block two-on-one"))

;;; d.
;;; Modify the CHOOSE-BEST-MOVE function so that it tries these two defensive strategies before choosing a move at random.
(defun choose-best-move (board)
  "First version"
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))

;;; e.
;;; If the computer goes first, then after the opponent's first move there may be an opportunity for the computer to set up a squeeze play or two-on-one situation to trap the opponent.
;;; Write functions to check the diagonals and suggest an appropriate attack if the opportunity exists. Modify the CHOOSE-BEST-MOVE function to include these offensive strategies in its list of things to try.
(defun try-squeeze-play (board)
  (sq-and-2 board *opponent* nil 11
            "set up a squeeze play"))

(defun try-two-on-one (board)
  (sq-and-2 board *computer* nil 11
            "set up a two-on-one"))

;;; Ex 10.9
;;; Write a destructive function CHOP that shortens any non-NIL list to a list of one element.
;;; (CHOP '(FEE FIE FOE FUM)) should return (FEE).
(defun chop (l)
  (if (consp l) (setf (cdr l) nil))
  l)




