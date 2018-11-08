;;;; CL, Gentle Introduction to symbolic computation 
;;; Chap. 2 exercises

;; Ex 2.8
; Show how to write MY-THIRD using FIRST ans two RESTs.
(defun my-third (a-list)
  (first (rest (rest a-list))))

;; Ex 2.9
; Show how to write MY-THIRD using SECOND.
(defun my-third-b (a-list)
  (second (rest a-list)))

;; Ex 2.13
; Write the functions to get each word in the list: (((FUN)) (IN THE) (SUN))

(defun get-first (my-list)
  (caaar my-list))

(defun get-second-first (my-list)
  (caadr my-list))

(defun get-second-second (my-list)
  (cadadr my-list))

(defun get-third (my-list)
  (caaddr my-list))
