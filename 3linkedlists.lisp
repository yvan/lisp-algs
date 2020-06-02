(ql:quickload :rutils)
(in-package :rtl-user)
(named-readtables:in-readtable rutils-readtable)

(defstruct list-cell
  data
  next)

(defstruct our-own-list
  (head nil :type (or list-cell null))
  (tail nil :type (or list-cell null))
  (size 0 :type (integer 0)))

(defun dwim-map (fn seq &rest seqs)
  (apply 'map (type-of seq) fn seqs))

(defun simple-mapcar (fn list)
  (let ((rez ()))
    (dolist (item list)
      (:= rez (cons (call fn item) rez)))
    (reverse rez)))

(defstruct (list-cell2 (:include list-cell))
  prev)

(defun our-cons2 (data lst)
  (when (null lst) (:= lst (make-our-own-list)))
  (let ((new-head (make-list-cell2
		   :data data
		   :next @lst.head)))
    (when @lst.head
      (:= @lst.head.prev new-head))
    (make-our-own-list
     :head new-head
     :tail @lst.tail
     :size (1+ @lst.size))))
