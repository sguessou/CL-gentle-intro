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

;;; e.
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
 
;;; alternative solution
(defun process-node (name)
  (let ((nd (find-node name)))
    (if nd
        (if (y-or-n-p "~&~A "
                      (node-question nd))
            (node-yes-case nd)
            (node-no-case nd))
        (format t
                "~&Node ~S not yet defined." name))))

;;; f.
;;; Write the function RUN. It maintains a local variable named CURRENT-NODE, whose initial value is START.
;;; It loops, calling PROCESS-NODE to process the current node, and storing the value returned by PROCESS-NODE back into CURRENT-NODE.
;;; If the value returned is a string, the function prints the string and stops. If the value returned is NIL, it also stops.
(defun run ()
  (do ((current-node 'start (process-node current-node)))
      ((or (stringp current-node)
           (null current-node))
       (format t "~&~A" current-node))))

;;; alternative solution
(defun run ()
  (do ((current-node 'start
                     (process-node current-node)))
      ((null current-node) nil)
    (cond ((stringp current-node)
           (format t "~&~A" current-node)
           (return nil)))))

;;; g.
;;; Write an interactive function to add a new node. It should prompt the user for the node name, the question, and the yes and no actions.
;;; Remember that the question must be a string, enclosed in double quotes.
;;; Your function should add the new node to the net.
(defun get-node-data ()
  (format t "~&Enter the node's name: ")
  (let ((name (read)))
    (format t "~&Enter the node's question: ")
    (let ((question (read)))
      (format t "~&Enter the yes action: ")
      (let ((yes (read)))
        (format t "~&Enter the no action: ")
        (let ((no (read)))
          (add-node name question yes no))))))


;;; Alternative solution
(defun interactive-add ()
  (let ((question nil)
        (name nil)
        (yes-case nil)
        (no-case nil))
    (format t "~&Name? ")
    (setf name (read))
    (format t "~&Question? ")
    (setf question (read))
    (format t "~&Yes action? ")
    (setf yes (read))
    (format t "~&No action? ")
    (setf no (read))
    (add-node name question yes no)))
