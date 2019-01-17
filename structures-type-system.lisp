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
  (setf *NODE-LIST* nil)
  'initialized)

;;; c.
;;; Write ADD-NODE. It should return the name of the node it added.
(defun add-node (name question yes-case no-case)
  (push (make-node :name name
             :question question
             :yes-case yes-case
             :no-case no-case)
        *NODE-LIST*)
  name)

;;; d. Write FIND-NODE, which takes a node name as input and returns the node if it appears in *NODE-LIST*, or NIL if it doesn't.
(defun find-node (name)
  (dolist (n *NODE-LIST*)
    (when (equal name (node-name n))
      (return n))))

;;; Write PROCESS-NODE. It takes a node name as input. If it can't find the node, it prints a message that the node hasn't been defined yet, and returns NIL. 
;;; Otherwise it asks the user the question associated with that node, and then returns the node's yes action or no action depending on how the user responds.
(defun process-node (name)
  (let ((n (find-node name)))
    (cond ((null n)
           (format t "~&The node hasn't been defined yet."))
          (t (format t "~&~A " (node-question n)) 
             (node-question n)
             (let ((ans (read)))
               (cond ((equal ans 'yes) (node-yes-case n))
                     ((equal ans 'no) (node-no-case n))
                     (t (process-node name))))))))

