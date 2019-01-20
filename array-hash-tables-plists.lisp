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
