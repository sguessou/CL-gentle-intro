;;; Chapter 6 - List Data Structures
;;; Exercises

;;; Ex 6.6
;;; Use the LAST function to write a function called LAST-ELEMENT that returns the last element of a list instead of the last cons cell.
;;; Write another version of LAST-ELEMENT using REVERSE instead of LAST. Write another version using NTH and LENGTH.
(defun last-element (l)
  "Returns last element of list version 1"
  (car (last l)))

(defun last-element (l)
  "Returns last element of list version 2"
  (car (reverse l)))

(defun last-element (l)
  "Returns last element of list version 3"
  (nth (- (length l) 1) l))

;;; Ex 6.7
;;; Use REVERSE to write a NEXT-TO-LAST function that returns the next-to-last element of a list. Write another version using NTH.
(defun next-to-last (l)
  (cadr (reverse l)))

(defun next-to-last (l)
  (nth (- (length l) 2) l)) 

;;; Ex 6.8
;;; Write a function MY-BUTLAST that returns a list with the last element removed.
;;; (MY-BUTLAST '(ROSES ARE RED)) should return the list (ROSES ARE). (MY-BUTLAST '(G A G A)) should return (G A G).
(defun my-butlast (l)
  (reverse (rest (reverse l))))

 
;;; Ex 6.9
;;; What primitive function does the following reduce to?
(defun mystery (x)
  (first (last (reverse x)))) 
;;; answer: CAR

;;; Ex 6.10
;;; A palindrome is a sequence that reads the same forward and backwards. The list (A B C D C B A) is a palindrome; (A B C A B C) is not.
;;; Write a function PALINDROMEP that returns T if its input is a palindrome.
(defun palindromep (l)
  (equal l (reverse l))) 

;;; Ex 6.11
;;; Write a function MAKE-PALINDROME that makes a palindrome out of a list, for example, given (YOU AND ME) as input it should return (YOU AND ME ME AND YOU).
(defun make-palindrome (l)
  (append l (reverse l)))

;;; Ex 6.15
;;; We can use MEMBER to write a predicate that returns a true value if a sentence contains the word "the."
(defun contains-the-p (sent)
  (member 'the sent))
;;; Suppose we instead want a predicate CONTAINS-ARTICLE-P that returns a true value if a sentence contains any article, such as "the," "a," or "an." Write a version of this predicate using INTERSECTION. 
;;; Write another version using MEMBER and OR. Could the problem be solved with AND instead or OR?
(defun contains-article-p (sent)
  (intersection '(the a an) sent))

(defun contains-article-p (sent)
  (or (member 'the sent) (member 'a sent) (member 'an sent)))

(defun contains-article-p (sent)
  (not (and  (not (member 'the sent)) 
             (not (member 'a sent))  
             (not (member 'an sent)))))

;;; Ex 6.18
;;; Write a function ADD-VOWELS that takes a set of letters as input and adds the vowels (A E I O U) to the set. 
;;; For example, calling ADD-VOWELS on the set (X A E Z) should produce the set (X A E Z I O U), except that the exact order of the elements in the result is unimportant.
(defun add-vowels (s)
  (union s '(a e i o u)))

;;; Ex 6.21
;;; If set x is a subset of set y, then subtracting y from x should leave the empty set.
;;; Write MY-SUBSETP, a version of the SUBSETP predicate that returns T if its first input is a subset of its second input.
(defun my-subsetp (a b)
  (not (set-difference a b)))

;;; Ex 6.24
;;; Sets are said to be equal if they contain exactly the same elements.
;;; Order does not matter in a set, so the sets (RED BLUE GREEN) and (GREEN BLUE RED) are considered equal.
;;; However, the EQUAL predicate does not consider them equal, because it treats them as lists, not as sets. Write a SET-EQUAL predicate that returns T if two things are equal as sets.
;;; (Hint: If two sets are equal, then each is a subset of the other.)
(defun set-equal (a b)
  (and (subsetp a b) (subsetp b a))) 

;;; Ex 6.25
;;; A set X is a proper subset of a set Y if X is a subset of Y but not equal to Y.
;;; Thus, (A C) is a proper subset of (C A B). (A B C) is a subset of (C A B), but not a proper subset of it.
;;; Write the PROPER-SUBSETP predicate, which returns T if its first input is a proper subset of its second input.
(defun proper-subset (a b)
  (and (subsetp a b) (not (subsetp b a)))) 

;;; Ex 6.26
;;; We are going to write a program that compares the descriptions of two objects and tells how many features they have in common.
;;; The descriptions will be represented as a list of features, with the symbol -VS- separating the first object from the second.
;;; Thus, when given a list like (large red shiny cube -vs- small shiny red four-sided pyramid)
;;; the program will respond with (2 COMMON FEATURES). We will compose this program from several small functions that you will write and test one at a time.
;;; a.
;;; Write a function RIGHT-SIDE that returns all the features to the right of the -VS- symbol. RIGHT-SIDE of the list shown above should return (SMALL SHINY RED FOUR-SIDED PYRAMID).
;;; Test your function to make sure it works correctly.
(defun right-side (o)
  (cdr (member '-vs- o)))

;;; b.
;;; Write a function LEFT-SIDE that returns all the features to the left of the -VS-. You can't use the MEMBER trick directly for this one.
(defun left-side (o)
  (cdr (member '-vs- (reverse o))))

;;; c.
;;; Write a function COUNT-COMMON that returns the number of features the left and right sides of the input have in common.
(defun count-common (o)
  (length (intersection (left-side o) (right-side o))))

;;; d.
;;; Write the main function, COMPARE, that takes a list of features describing two objects, with a -VS- between them, and reports the number of features they have in common.
;;; COMPARE should return a list of form (n COMMON FEATURES).
(defun compare (o)
  (cons (count-common o) '(common features)))

;;; Ex 6.30
;;; Make a table called BOOKS of five books and their authors. The first entry might be (WAR-AND-PEACE LEO-TOLSTOY)
(defvar *books*
      '((2666 Roberto-Bolano)
        (Crimes-and-Punishments Fedor-Dostoievsky)
        (Kolyma-Tales Varlam-Shalamov)
        (Valis Philip-K-Dick)
        (Politics Aristotle)))

;;; Ex 6.31
;;; Write the function WHO-WROTE that takes the name of a book as input and returns the book's author.
(defun who-wrote (b)
  (cadr (assoc b *books*)))

;;; Ex 6.35
;;; In this problem we will simulate the behaviour of a very simple-minded creature, Nerdus Americanis (also known as Computerus Hackerus).
;;; This creature has only five states: Sleeping, Eating, Waiting-for-a-Computer, Programming, and Debugging. Its behavior is cyclic: After it sleeps it always eats, after it eats it always waits for a computer, and so on, until after debugging it goes back to sleep for a while.
;;; a. Write a data structure for the five-state cycle given above, and store it in a global variable called NERD-STATES.
(defvar NERD-STATES 
  '(Sleeping Eating Waiting-for-a-computer Programming Debugging)) 

;;; b. Write a function NERDUS that takes the name of a state as input and uses the data structure you designed to determine the next state the creature will be in.
;;; (NERDUS 'SLEEPING) should return EATING, for example.
;;; (NERDUS 'DEBUGGING) should return SLEEPING.
(defun nerdus (s)
  (if (eq s 'debugging) 
      'sleeping
      (cadr (member s nerd-states))))

;;; c. What is the result of (NERDUS 'PLAYING-GUITAR)?
NIL

;;; d. When Nerdus Americanis ingests too many stimulants (caffeine overdose), it stops sleeping. After finishing Debugging, it immediately goes on to state Eating.
;;; Write a function SLEEPLESS-NERD that works just like NERDUS except it never sleeps.
;;; Your function should refer to the global variable NERD-STATES, as NERDUS does.
(defun sleepless-nerd (s)
  (let ((n (nerdus s)))
    (if (eq n 'sleeping)
      'eating
      n)))
  
;;; e. Exposing Nerdus Americanis to extreme amounts of chemical stimulants produces pathological behavior. Instead of an orderly advance to its next state, the creature advances two states. 
;;; For example, it goes from Eating directly to Programming, and from there to Sleeping. Write a function NERD-ON-CAFFEINE that exhibits this unusual pathology. Your function should use the same table as NERDUS.
(defun nerd-on-caffeine (s)
  (cond ((eq s 'programming) 'sleeping)
          ((eq s 'debugging) 'eating)
          (t (nerdus (nerdus s))))) 

;;; Ex 6.36
;;; Write a function to swap the first and last element of any list. (SWAP-FIRST-LAST '(YOU CANT BUY LOVE)) should return (LOVE CANT BUY YOU).
(defun swap-first-last (x)
  (let* ((f (reverse (rest x)))
         (l (reverse (rest f))))
    (cons (first f) (append l (list (first x))))))

;;; Ex 6.37
;;; ROTATE-LEFT and ROTATE-RIGHT are functions that rotate the elements of a list.
;;; (ROTATE-LEFT '(A B C D E)) returns (B C D E A), whereas ROTATE-RIGHT returns (E A B C D). Write these functions.
(defun rotate-left (x)
  (append (rest x) (list (first x))))

(defun rotate-right (x)
  (let* ((l (first (reverse x)))
        (r (rest (reverse x))))
    (append (list l) (reverse r))))

;;; Ex 6.41
;;; Table rooms containing layout of the house
(defvar rooms 
  '((library (east upstairs-bedroom) (south back-stairs))
    (back-stairs (north library) (south downstairs-bedroom))
    (downstairs-bedroom (north back-stairs) (east dinin-room))
    (upstairs-bedroom (west library) (south front-stairs))
    (front-stairs (north upstairs-bedroom) (south living-room))
    (living-room (north front-stairs) (east kitchen) (south dining-room))
    (dining-room (north living-room) (west downstairs-bedroom) (east pantry))
    (kitchen (west living-room) (south pantry))
    (pantry (north kitchen) (west dining-room))))

;;; a.
;;; Write a function CHOICES that take the name of a room as input and returns the table of permissible directions Robbie the Robot may take from that room.
;;; For example (CHOICES 'PANTRY) should return the list ((NORTH KITCHEN) (WEST DINING-ROOM)).
;;; Test your function to make sure it returns the correct result. 
(defun choices (n)
  (rest (assoc n rooms)))

;;; b.
;;; Write a function LOOK that takes two inputs, a direction and a room, and tells where Robbie would end up if he moved in that direction from that room. For example, (LOOK 'NORTH 'PANTRY) should return KITCHEN.
;;; (LOOK 'SOUTH 'PANTRY) should return NIL. Hint: The CHOICES function will be a useful building block.
(defun look (d r)
  (rest (assoc d (choices r))))

;;; c.
;;; We will use the global variable LOC to hold Robbie's location. Type in an expression to set his location to be the pantry. The following function should be used whenever you want to change his location.
(defun set-robbie-location (p)
  "Moves Robbie to PLACE by setting the variable LOC."
  (setf loc p))

(set-robbie-location 'pantry)

;;; d.
;;; Write a function HOW-MANY-CHOICES that tells how many choices Robbie has for where to move to next. Your function should refer to the global variable LOC to find his current location. If he is in the pantry, (HOW-MANY-CHOICES) should return 2.
(defun how-many-choices ()
  (length (choices loc)))

;;; e.
;;; Write a predicate UPSTAIRSP that returns T if its input is an upstairs location. (The library and the upstairs bedroom are the only two locations upstairs.) 
;;; Write a predicate ONSTAIRSP that returns T if its input is either FRONT-STAIRS or BACK-STAIRS.
(defun upstairsp (l)
  (not (not (member l '(library upstairs-bedroom)))))

(defun onstairsp (l)
  (or (equal l 'front-stairs)
      (equal l 'back-stairs)))

;;; f.
;;; Where's Robbie? Write a function of no inputs called WHERE that tells where Robbie is. If he is in the library, (WHERE) should say (ROBBIE IS UPSTAIRS IN THE LIBRARY). If he is in the kitchen, it should say (ROBBIE IS DOWNSTAIRS IN THE KITCHEN).
;;; If he is on the front stairs, it should say (ROBBIE IS ON THE FRONT-STAIRS).
(defun where ()
  (cond ((upstairsp loc) (append '(robbie is upstairs in the) (list loc)))
        ((and (not (upstairsp loc)) 
              (not (onstairsp loc))) 
         (append '(robbie is downstairs in the) (list loc)))
        ((onstairsp loc) (append '(robbie is on the) (list loc)))))
