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

