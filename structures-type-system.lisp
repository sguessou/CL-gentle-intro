;;; Chapter 12 - Structures and The Type System

;;; Exercises

;;; Ex 12.4
;;; In this exercise we will create a discrimination net for automotive diagnosis that mimics the behaviour of the system shown before.

;;; a.
;;; Write a DEFSTRUCT for a structure called NODE, with four components called NAME,QUESTION, YES-CASE, and NO-CASE.
(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))

;;; b.
;;; Define a global variable *NODE-LIST* that will hold all the nodes in the discrimination net. Write a function INIT that initializes the network by setting *NODE-LIST* to NIL.
(setf *NODE-LIST* nil)

(defun init ()
  (setf *NODE-LIST* nil))

;;; c.
;;; Write ADD-NODE. It should return the name of the node it added.
(defun add-node (s1 q1 q2 s2)
  (make-node :name s1
             :question q1
             :yes-case q2
             :no-case s2)
  s1)


