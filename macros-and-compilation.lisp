;;; Chapter 14 - Macros and Compilation

;;; Exercises

;;; Ex 14.3
;;; Write a SET-NIL macro that sets a variable to NIL.
(defmacro set-nil (var)
  (list 'setq var nil))

;;; Ex 14.4
;;; Write a macro called SIMPLE-ROTATEF that switches the value of two variables.
;;; for example, if A is two and B is seven, then (SIMPLE-ROTATEF A B) shoul make A seven and B two.
;;; Obviously, setting A to B first, and then setting B to A won't work.
;;; Your macro should expand into a LET expression that holds on to the original values of the two variables and then assigns them their new values in its body.
(defmacro simple-rotatef (a b)
  `(let ((clone-a ,a)
         (clone-b ,b))
    (setf ,a clone-b)
    (setf ,b clone-a))) 

;;; Ex 14.5
;;; Write a macro SET-MUTUAL that takes two variable names as input and expands into an expression that sets each variable to the name of the other. 
;;; (SET-MUTUAL A B) should set A to 'B, and B to 'A.
(defmacro set-mutual (a b)
  `(let ((var-a ',b)
         (var-b ',a))
     (setf ,a var-a)
     (setf ,b var-b)))

;;; Ex 14.6
;;; Write a macro called VARIABLE-CHAIN that accepts any number of inputs.
;;; The expression (VARIABLE-CHAIN A B C D) should expand into an expression that sets A to 'B, B to 'C, and C to 'D.
(defmacro set-zero (&rest vars)
  `(progn ,@(mapcar #'(lambda (var)
                        (list 'setf var 0))
                    vars)
          '(zeroed ,@vars)))

(defmacro variable-chain (&rest vars)
  `(progn ,@(mapcar #'(lambda (a b)
              `(setf ,a ',b))
          vars
          (rest vars))
          '(done ,@vars)))

;;; Alternative solution
(defmacro variable-chain (&rest vars)
  `(progn
    ,@(do ((v vars (rest v))
            (res nil))
           ((null (rest v)) (reverse res))
         (push `(setf ,(first v)
                      ',(second v))
               res))))

;;; CASE STUDY: Finite State Machines
(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  (format stream "#<Node ~A>"
          (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (format stream "#<ARC ~A / ~A / ~A>"
          (node-name (arc-from arc))
          (arc-label arc)
          (node-name (arc-to arc))))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defmacro defnode (name)
  `(add-node ', name))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node named ~A exists." name)))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (new-arc (make-arc :from from
                            :label label
                            :to to
                            :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
          (nconc (node-outputs from)
                 (list new-arc)))
    (setf (node-inputs to)
          (nconc (node-inputs to)
                 (list new-arc)))
    new-arc))

(defun fsm (&optional (starting-point 'start))
  (setf *current-node* (find-node starting-point))
  (do ()
      ((null (node-outputs *current-node*)))
    (one-transition)))

(defun one-transition ()
   (format t "~&State ~A. Input: "
           (node-name *current-node*))
   (let* ((ans (read))
          (arc (find ans
                     (node-outputs *current-node*)
                     :key #'arc-label)))
     (unless arc
       (format t "~&No arc from ~A has label ~A.~%"
               (node-name *current-node*) ans)
       (return-from one-transition nil))
     (let ((new (arc-to arc)))
       (format t "~&~A" (arc-action arc))
       (setf *current-node* new))))

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode end)

(defarc start  nickel       have-5  "Clunk!")
(defarc start  dime         have-10 "Clink!")
(defarc start  coin-return  start   "Nothing to return!")

(defarc have-5  nickel      have-10 "Clunk!")
(defarc have-5  dime        have-15 "Clink!")
(defarc have-5  coin-return start   "Returned five cents.")

(defarc have-10 nickel      have-15 "Clunk!")
(defarc have-10 dime        have-20 "Clink!")
(defarc have-10 coin-return start   "Returned ten cents.")

(defarc have-15 nickel      have-20 "Clunk!")
(defarc have-15 dime        have-20 "Nickel change.")
(defarc have-15 gum-button  end     "Deliver gum.")
(defarc have-15 coin-return start   "Returned fifteen cents.")

(defarc have-20 nickel      have-20 "Nickel returned.")
(defarc have-20 dime        have-20 "Dime returned.")
(defarc have-20 gum-button  end     "Deliver gum, nickel change.")

(defarc have-20 mint-button end     "Deliver mints.")
(defarc have-20 coin-return start   "Returned twenty cents.")

;;; Ex 14.7
;;; Extend the vending machine example to sell chocolate bars for 25 cents.
;;; Make it accept quarters as well as nickels and dimes.
;;; When you put in a quarter it should go "Ker-chunck!"

(defnode have-25)
(defnode have-30)
(defnode have-35)
(defarc start    quarter       have-25  "Ker-chunck!")
(defarc have-5   quarter       have-30  "Ker-chunck!")
(defarc have-10  quarter       have-35  "Ker-chunck!")

(defarc have-25  choc-button   end      "Deliver chocolate.")
(defarc have-30  choc-button   end      "Deliver chocolate, nickel change.")
(defarc have-35  choc-button   end      "Deliver chocolate, dime change.")

(defarc have-25  coin-return   start   "Returned twenty five cents.")
(defarc have-30  coin-return   start   "Returned thirty cents.")
(defarc have-35  coin-return   start   "Returned thirty five cents.")

;;; Ex 14.11
;;; In this keyboard exercise we will write a compiler for finite state machines that turns each node into a function.
;;; The definition of the vending machine's nodes and arcs should already be loaded into your Lisp before beginning the exercise.

;;; a.
;;; Write a function COMPILE-ARC that takes an arc as input and returns a COND clause, following the example shown previously.
;;; Test your function on some of the elements in the list *ARCS*.
;;; (COMPILE-ARC (FIRST *ARCS*)) should return this list:
((equal this-input 'nickel)
 (format t "~&~A" "Clunk!")
 (have-5 (rest input-syms)))

(defun compile-arc (arc)
  `((equal this-input ',(arc-label arc))
    (format t "~&~A" ,(arc-action arc))
    (,(node-name (arc-to arc)) (rest input-syms))))

;;; b.
;;; Write a function COMPILE-NODE that takes a node as input and returns a DEFUN expression for that node.
;;; (COMPILE-NODE (FIND-NODE 'START)) should return the DEFUN shown previously.
(defun compile-node (node)
  `(defun ,(node-name node) (input-syms
                             &aux (this-input (first input-syms)))
     (cond ((null input-syms) ',(node-name node))
           ,@(cn-helper node)
           (t (format t "No arc for ~A with label ~A."
                     ',(node-name node) this-input)))))

(defun cn-helper (node)
  (do ((result nil)
       (arcs (node-outputs node) (rest arcs)))
       ((null arcs) result)
    (setf result 
          (cons (compile-arc (first arcs)) result))))

;;; Book's solution
(defun compile-node (node)
  (let ((name (node-name node))
        (arc-clauses
         (mapcar #'compile-arc
                 (node-outputs node))))
    `(defun ,name (input-syms
                   &aux (this-input
                         (first input-syms)))
       (cond ((null input-syms) ',name)
             ,@arc-clauses
             (t (format t
                        "~&There is no arc from ~A with label ~S"
                        ',name this-input))))))
;;; c.
;;; Write a macro COMPILE-MACHINE that expands into a PROGN containing a DEFUN for each node in *NODES*.
(defmacro compile-machine ()
  `(progn ,@(mapcar #'compile-node *nodes*)))

;;; d.
;;; Compile the vending machine. What does the expression (START '(DIME DIME DIME GUM-BUTTO)) produce?
Clink!
Clink!
Dime returned.
Deliver gum, nickel change.
End
