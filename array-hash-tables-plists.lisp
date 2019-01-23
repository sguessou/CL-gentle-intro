;;; Chapter 13 - Arrays, Hash Tables, And Property Lists

;;; Exercises

;;; Ex 13.1
;;; Write a function called SUBPROP that deletes an element from a set stored under a property name. 
;;; For example, if the symbol ALPHA has the list (A B C D E) as the value of its FOOPROP property, doing (SUBPROP 'ALPHA 'D 'FOOPROP) should leave (A B C E) as the value of ALPHA's FOOPROP property.
(setf (get 'alpha 'fooprop) '(a b c d e))

(defun subprop (name element prop)
  (setf (get name prop)
        (remove element
                (get name prop))))

;;; Ex 13.2
;;; Write a function called FORGET-MEETING that forgets that two particular persons have ever met each other.
;;; Use SUBPROP in your solution.
(defun addprop (sym elem prop)
  (pushnew elem (get sym prop)))

(defun record-meeting (x y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)

(symbol-plist 'john)

(defun forget-meeting (x y)
  (remprop x 'has-met)
  (remprop y 'has-met)
  t)

;;; Alternative solution
(defun forget-meeting (person1 person2)
  (subprop person1 person2 'has-met)
  (subprop person2 person1 'has-met)
  'forgotten)

;;; Ex 13.3
;;; Using SYMBOL-PLIST, write your own version of the GET function.
(defun my-get (symbol property)
  (do ((p (symbol-plist symbol) (cddr p)))
      ((null p) nil)
    (if (equal property (first p))
        (return (second p)))))


;;; Ex 13.4
;;; Write a predicate HASPROP that returns T or NIL to indicate whether a symbol has a particular property, independent of the value of that property.
;;; Note: If symbol A has a property FOO with value NIL, (HASPROP 'A 'FOO) should still return T.
(defun hasprop (symbol property)
  (do ((p (symbol-plist symbol) (cddr p)))
      ((null p) nil)
    (if (equal property (first p))
        (return t))))

;;; Ex 13.8
;;;  Follow the steps below to create a histogram-drawing program. Your functions should not assume that the histogram will have exactly eleven bins.
;;; In other words, don't use eleven as a constant in you program; use (LENGTH *HIST-ARRAY*) instead.
;;; That way your program will be able to generate histograms of any size.

;;; a.
;;; Write expressions to set up a global variable *HIST-ARRAY* that holds the array of counts, and a global variable *TOTAL-POINTS* that holds the number of points recorded so far.
(setf *hist-array* nil)

(setf *total-points* 0)

;;; b.
;;; Write a function NEW-HISTOGRAM to initialize these variables appropriately.
;;; It should take one input: the number of bins the histogram is to have. 
(defun new-histogram (n)
  (setf *total-points* 0)
  (setf *hist-array* 
        (make-array n 
                    :initial-element 0))
  t)

;;; c.
;;; Write the function RECORD-VALUE that takes a number as input.
;;; If the number is between zero and ten, it should increment the appropriate element of the array, and also update *TOTAL-POINTS*.
;;; If the input is out of range, RECORD-VALUE should issue an appropriate error message.
(defun record-value (n)
  (cond ((or (< n 0) (> n 10))
         (format t "~&Number ~A is out of range." n))
        (t (setf (aref *hist-array* n)
                 (+ (aref *hist-array* n) 1))
           (dotimes (i 11 *total-points*)
             (setf *total-points* 
                   (+ *total-points* 
                      (aref *hist-array* i)))))))

;;; Better alternative
(defun record-value (v)
  (incf *total-points*)
  (if (and (>= v 0)
           (< v (length *hist-array*)))
      (incf (aref *hist-array* v))
      (error "Value ~S out of bounds." v)))

;;; d.
;;; Write a function PRINT-HIST-LINE that takes a value from zero to ten as input, looks up that value in the array, and prints the corresponding line of the histogram.
;;; To get the numbers to line up in columns properly, you will need to use the format directives ~2S to display the value and ~3S to display the count. 
;;; You can use a DOTIMES to print the asterisks.
(defun print-hist-line (n)
  (let ((cnt (aref *hist-array* n)))
    (format t "~&~2D [~3D] " n cnt)
    (dotimes (i cnt)
      (format t "*"))))

;;; e.
;;; Write the function PRINT-HISTOGRAM.
(defun print-histogram (iterations)
  (new-histogram 11)
  (dotimes (i iterations)
    (record-value (random 11)))
  (dotimes (i 11)
    (print-hist-line i))
  (format t "~&    ~3D total" *total-points*))

;;; Ex 13.9
;;; Set up the global variable CRYPTO-TEXT as shown. Then build the cryptogram-solv§ing tool by following these instruction:

;;; a.
;;; Each letter in the alphabet has a corresponding letter to which it deciphers, for example, P deciphers to A.
;;; As we solve the cryptogram we will store this information in two hash tables called *ENCIPHER-TABLE* and *DECIPHER-TABLE*.
;;; We will use *DECIPHER-TABLE* to print out the deciphered cryptogram.
;;; We need *ENCIPHER-TABLE* to check for two letters being deciphered to the same thing, for example, if P is deciphered to A and then we tried to decipher K to A, a look at *ENCIPHER-TABLE* would reveal that A had already been assigned to P.
;;; Similarly, if P is deciphered to A and then we tried deciphering P to E, a look at *DECIPHER-TABLE* would tell us that P had already been deciphered to A.
;;; Write expressions to initialize these global variables.
(setf *decipher-table* (make-hash-table))

(setf *encipher-table* (make-hash-table))

(setf crypto-text
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
        "enlpo pib slafml pvv bfwkj"))

;;; b.
;;; Write a function MAKE-SUBSTITUTION that takes two character objects as input and stores the appropriate entries in *DECIPHER-TABLE* and *ENCIPHER-TABLE* so that the first letter deciphers to the second and the second letter enciphers to the first.
;;; This function does not need to check if either letter already has an entry in these hash tables.
(defun make-substitution (a b)
  (setf (gethash a *decipher-table*) b)
  (setf (gethash b *encipher-table*) a)
  'make-substituion-ok)

;;; c.
;;; Write a function UNDO-SUBSTITUTION that takes one letter as input. 
;;; It should set the *DECIPHER-TABLE* entry of that letter, and the *ENCIPHER-TABLE* entry of the letter it deciphered to, to NIL.
(defun undo-substitution (l)
  (let ((decipher-to (gethash l *decipher-table*)))
    (setf (gethash l *decipher-table*) nil)
    (setf (gethash decipher-to *encipher-table*) nil))
  'undo-substituion-ok)

;;; d.
;;; Look up the documentation for the CLRHASH function, and write a function CLEAR that clears the two hash tables used in this problem.
(defun clear ()
  (clrhash *decipher-table*)
  (clrhash *encipher-table*)
  'clear-ok)

;;; e.
;;; Write a function DECIPHER-STRING that takes a single encoded string as input and return a new, partially decoded string. It should begin by making a new string the same length as the input, containing all spaces.
;;; Here is how to do that, assuming the variable LEN holds the length: (make-string len :initial-element #\Space).
;;; Next the function should iterate through the elements of the input string, which are character objects. For each character that deciphers to something non-NIL, that value should be inserted into the corresponding position in the new string.
;;; Finally, the function should return the new string.
;;; When testing this function, make sure its inputs are all lowercase. 
(defun decipher-string (str)
  (do* ((len (length str)) 
        (new-str (make-string len 
                              :initial-element #\Space))
        (i 0 (1+ i)))
       ((equal i len) new-str)
    (let* ((char (aref str i))
           (new-char 
             (gethash char *decipher-table*)))
      (when new-char
        (setf (aref new-str i) new-char))))) 

;;; f.
;;; Write a function SHOW-LINE that displays one line of cryptogram text, with the deciphered text displayed beneath it.
(defun show-line (line)
  (format t "~&~A" line)
  (format t "~&~A"
          (decipher-string line)))

;;; g.
;;; Write a function SHOW-TEXT that takes a cryptogram (list of strings) as input and displays the lines as in the examples at the beginning of this exercise.
(defun show-text (cryptogram)
  (dolist (element cryptogram)
    (format t "~&~A" element)))

;;; h.
;;; Type in the definition of GET-FIRST-CHAR, which returns the first character in the lowercase printed of an object.
(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0)))

;;; i.
;;; Write a function READ-LETTER that reads an object from the keyboard. If the object is the symbol END or UNDO, it should be returned as the value of READ-LETTER.
;;; Otherwise READ-LETTER should use GET-FIRST-CHAR on the object to extract the first character of its printed representation; it should return that character as its result.
(defun read-letter ()
  (do ((answer nil))
      (nil)
    (setf answer (read))
    (if (or (equal answer 'end)
            (equal answer 'undo))
        (return answer)
        (return (get-first-char answer)))))
