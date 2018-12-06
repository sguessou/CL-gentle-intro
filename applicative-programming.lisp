;;; Chapter 7 - Applicative Programming
;;; Exercises

;;; Ex 7.1
;;; Write an ADD1 function that adds one to its input. Then write an expression to add one to each of the list (1 3 5 7 9)
(defun add1 (n)
  (+ n 1))

(mapcar #'add1 '(1 3 5 7 9))

;;; Ex 7.2
let the global variable DAILY-PLANET contain the following table:
((olsen jimmy 123-76-4535 cub-reporter)
 (kent  clark 089-52-6787 reporter)
 (lane  lois  951-26-1438 reporter)
 (white perry 355-16-7439 editor))
;;; Each table entry consist of a last name, a first name, a social security number, and a job title. use MAPCAR on this table to extract a list of social numbers.
(setf daily-planet 
     '((olsen jimmy 123-76-4535 cub-reporter)
       (kent  clark 089-52-6787 reporter)
       (lane  lois  951-26-1438 reporter)
       (white perry 355-16-7439 editor)))

(mapcar #'third daily-planet)

;;; Write an expression to apply the ZEROP predicate to each element of the list (2 0 3 4 0 -5 -6).
;;; The answer you get should be a list of Ts and NILs.
(mapcar #'zerop '(2 0 3 4 0 -5 -6))

;;; Suppose we want to solve a problem similar to the preceding one, but instead of testing wheter an element is zero, we want to test whether it is greater than five.
;;; We can't use > directly for this because > is a function of two inputs; MAPCAR  will only give it one input.
;;; Show how first writing a one-input function called GREATER-THAN-FIVE-P would help.
(defun greater-than-five-p (n)
  (> n 5))

(mapcar #'greater-than-five-p '(2 0 3 4 0 -5 -6))

;;; Ex 7.5
;;; Write a lambda expression to subtract seven from a number
#'(lambda (n) (- n 7))

;;; Ex 7.6
;;; Write a lambda expression that returns T if its input is T or NIL, but NIL for any other input.
#'(lambda (i) (cond (or (eq t i) (eq nil i)) t))

;;; Ex 7.7
;;; Write a function that takes a list such as (UP DOWN UP UP) and "flips" each element, returning (DOWN UP DOWN DOWN). 
;;; Your function should include a lambda expression that knows how to flip an individual element, plus an applicative operator to do this to every element of the list.
(defun flip (l)
  (mapcar #'(lambda (e) 
              (if (eq e 'up) 
                  'down
                  'up)) l))

;;; Ex 7.8
;;; Write a function that takes two inputs, X and K, and returns the first number in the list X that is roughly equal to K.
;;; Let's say that "roughly equal" means no less than K-10 and no more than K+10.
(defun ex-7.8 (x k)
  (find-if #'(lambda (e)
               (and (>= e (- k 10)) (<= 2 (+ k 10))))
           x))

;;; Ex 7.9
;;; Write a function FIND-NESTED that returns the first element of a list that is itself a non-NIL list.
(defun find-nested (l)
  (find-if #'(lambda (e)
                  (and (listp e) (> (length e) 0)))
              l))

(defun find-nested (l)
  (find-if #'consp l))

;;; Ex 7.10
;;; In this exercise we will write a program to transpose a song from one key to another. In order to manipulate notes more efficiently, we will translate them into numbers.
;;; Here is the correspondence between notes and numbers for a non-octave scale:
C       = 1         F-SHARP = 7
C-SHARP = 2         G       = 8
D       = 3         G-SHARP = 9
D-SHARP = 4         A       = 10
E       = 5         A-SHARP = 11
F       = 6         B       = 12
;;; a.
;;; Write a table to represent this information. Store it in a global variable called NOTE-TABLE.
(setf note-table 
      '((c 1) (c-sharp 2) (d 3) (d-sharp 4) (e 5) (f 6)
        (f-sharp 7) (g 8) (g-sharp 9) (a 10) (a-sharp 11)
        (b 12)))
    
;;; b.
;;; Write a function called NUMBERS that takes a list of notes as input and returns the corresponding list of numbers. (NUMBERS '(E D C D E E E)) should return (5 3 1 3 5 5 5).
;;; This list represents the first seven notes of "Mary Had a Little Lamb."
(defun numbers (l)
  (mapcar #'(lambda (x)
              (cadr (assoc x note-table)))
          l))

;;; c.
;;; Write a function called NOTES that takes a list of numbers as input and returns the corresponding list of notes. (NOTES '(5 3 1 3 5 5 5)) should return (E D C D E E E). Hint: Since NOTE-TABLE is keyed by note, ASSOC can't look up numbers in it;
;;; neither can RASSOC, since the elements are lists, not dotted pairs. Write you own table-searching function to search NOTE-TABLE by number instead of by note.
(defun table-searching (x)
  (find-if #'(lambda (e)
              (eq (car (reverse e)) x))
           note-table))

(defun notes (l)
  (mapcar #'(lambda (x)
              (car x))
          (mapcar #'table-searching l)))

;;; e.
;;; To transpose a piece of music up by n half steps, we begin by adding the value n to each note in the piece. Write a function called RAISE that takes a number n and a list of numbers as input and raises each number in the list by the value n.
;;; (RAISE 5 '(5 3 1 3 5 5 5)) should return (10 8 6 8 10 10 10), whis is "Mary had a little lamb" transposed five half steps from the key of C to the key of F.
(defun raise (n l)
  (mapcar #'(lambda (x)
              (+ x n))
          l))

;;; f.
;;; Sometimes when we raise the value of a note, we may raise it right into the next octave. For instance, if we raise the triad C-E-G represented by the list (1 5 8) into the key of F by adding five to each note, we get (6 10 13), or F-A-C.
;;; Here the C note, represented by the number 13, is an octave above the regular C, represented by 1. Write a function called NORMALIZE that takes a list of numbers as input and "normalizes" them to make them be between 1 and 12.
;;; A number greater than 12 should have 12 subtracted from it; a number less than 1 should have 12 added to it. (NORMALIZE '(6 10 13)) should return (6 10 1).
(defun normalize (l)
  (mapcar #'(lambda (x)
              (cond ((> x 12) (- x 12))
                    ((< x 1) (+ x 12))
                    (t x)))
          l))

;;; g.
;;; Write a function TRANSPOSE that takes a number n and a song as input, and returns the song transposed by n half steps.
;;; (TRANSPOSES 5 '(E D C D E E E)) should return (A G F G A A A). Your solution should assume the availability of the NUMBERS, NOTES, RAISE and NORMALIZE functions. Try transposing "Mary Had a Little Lamb" up by 11 half steps. What happens if you transpose it by 12 half steps? How about -1 half steps?
(defun transpose (n l)
  (notes 
   (normalize 
    (raise n (numbers l)))))

;;; Ex 7.11
;;; Write a function to pick out those numbers in a list that are greater than one and less than five.
(defun ex-711 (l)
  (remove-if-not #'(lambda (x)
                     (and (> x 1) (< x 5)))
                 l))

;;; Ex 7.12
;;; Write a function that counts how many times the word "the" appears in a sentence.
(defun ex-712 (l)
  (length (remove-if-not #'(lambda (x)
                             (eq x 'the))
                         l)))

;;; Ex 7.13
;;; Write a function that picks from a list of lists those of exactly length two.
(defun ex-713 (l)
  (remove-if-not #'(lambda (x)
                     (eq (length x) 2))
                 l))

;;; Ex 7.14
;;; Here is a version of SET-DIFFERENCE written with REMOVE-IF:
(defun my-setdiff (x y)
  (remove-if #'(lambda (e)
                 (member e y))
             x))
;;; Show how the INTERSECTION and UNION functions can be written using REMOVE-IF or REMOVE-IF-NOT.
(defun my-intersection (x y)
  (remove-if-not #'(lambda (e)
                     (member e y))
                 x))

(defun my-union (x y)
  (append y 
          (remove-if #'(lambda (e)
                 (and (member e x) (member e y)))
             x)))

;;; Ex 7.15
;;; In this exercise we will manipulate cards with applicative operators. A card will be represented by a list of form (rank suit), for example, (ACE SPADES) or (2 CLUBS). A hand will be represented by a list of cards.

;;; a.
;;; Write the functions RANK and SUIT that return the rank and suit of a card, respectively. (RANK '(2 CLUBS)) should return 2, and (SUIT '(2 CLUBS)) should return CLUBS.
(defun rank (l)
  (car l))

(defun suite (l)
  (cadr l))

;;; b.
;;; Set the global variable MY-HAND to the following hand of cards:
((3 hearts) (5 clubs) (2 diamonds) (4 diamonds) (ace spades))
;;; Now write a function COUNT-SUIT that takes two inputs, a suit and a hand of cards, and returns the number of cards belonging to that suit. (COUNT-SUIT 'DIAMONDS MY-HAND) should return 2.
(setf my-hand '((3 hearts) (5 clubs) (2 diamonds) (4 diamonds) (ace spades))) 

(defun count-suit (s h)
  (length (remove-if-not #'(lambda (x)
                      (eq (suite x) s))
                         h)))

;;; c.
;;; Set the global variable COLORS to the following table:
((clubs black) (diamonds red) (hearts red) (spades black))
;;; Now write a function COLOR-OF that uses the table COLORS to retrieve the color of a card. (COLOR-OF '(2 CLUBS)) should return BLACK. (COLOR-OF '(6 HEARTS)) should return RED.
(setf colors '((clubs black) (diamonds red) (hearts red) (spades black)))

;;; Now write a function COLOR-OF that uses the table COLORS to retrieve the color of a card. (COLOR-OF '(2 CLUBS)) should return BLACK. 
(defun color-of  (c)
  (cadr (assoc (suite c) colors)))

;;; d.
;;; Write a function FIRST-RED that returns the first card of a hand that is of a red suit, or NIL if none are.
(defun first-red (h)
  (find-if #'(lambda (c)
                  (eq (color-of c) 'red))
              h))

;;; e.
;;; Write a function BLACK-CARDS that returns a list of all the black cards in a hand.
(defun black-cards (h)
  (remove-if-not #'(lambda (c)
                     (eq (color-of c) 'black))
                 h))

;;; f
;;; Write a function WHAT-RANKS that takes two inputs, a suit and a hand, and return the ranks of all cards belonging to that suit. 
;;; (WHAT-RANKS 'DIAMONDS MY-HAND) should return the list (2 4).
;;; (WHAT-RANKS 'SPADES MY-HAND) should return the list (ACE).
;;; Hint: First extract all the cards of the specified suit, then use another operator to get the ranks of those cards.
(defun what-ranks (s h)
  (mapcar #'(lambda (c) (rank c))
             (remove-if-not #'(lambda (c) (eq (suite c) s))
                            h)))

;;; g.
;;; Set the global variable ALL-RANKS to the list 
(2 3 4 5 6 7 8 9 10 jack queen king ace)
;;; Then write a predicate HIGHER-RANK-P that takes two cards as input and returns true if the first card has a higher rank than the second.
;;; Hint: look at the BEFOREP predicate on page 171 of Chapter 6.
(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (x y l)
  "Returns true if X appears before Y in L"
  (member y (member x l)))

(defun bang (e)
  "Retuns boolean value of e"
  (not (not e)))

(defun higher-rank-p (c1 c2)
   (bang 
    (beforep (rank c2) (rank c1) all-ranks)))

;;; h.
;;; Write a function HIGH-CARD that returns the highest ranked card in a hand. 
;;; Hint: One way to solve this is to use FIND-IF to search a list of ranks (ordered from high to low) to find the highest rank that appears in the hand.
;;; Then use ASSOC on the hand to pick the card with that rank. Another solution would be to use REDUCE (defined in the next section) to repeatedly pick the highest card of each pair.

(defun high-card (h)
  (reduce #'(lambda (x y)
              (if (higher-rank-p x y)
                  x
                  y))
          h))

;;; 7.16
;;; Suppose we had a list of sets ((A B C) (C D A) (F B D) (G)) that we wanted to collapse into one big set. If we use APPEND for our reducing function, the result won't be a true set, because some elements will appear more than once.
;;; What reducing function should be used instead?
(reduce #'union '((A B C) (C D A) (F B D) (G)))

;;; 7.17
;;; Write a function that, given a list of lists, returns the total length of all the lists. This problem can be solved two different ways.
(defun my-length (l)
  (reduce #'+ (mapcar #'(lambda (x)
                          (length x))
                      l)))

;;; 7.19
;;; Write a function ALL-ODD that returns T if every element of a list of numbers is odd.
(defun all-odd (l)
  (every #'oddp l))

;;; 7.20
;;; Write a function NONE-ODD that returns T if every element of a list of numbers is not odd.
(defun none-odd (l)
  (every #'evenp l))

;;; 7.21
;;; Write a function NOT-ALL-ODD that returns T if not every element of a list of numbers is odd.
(defun not-all-odd (l)
  (if (all-odd l)
      nil
      t))
