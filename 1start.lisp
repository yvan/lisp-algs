(ql:quickload :rutils)
(in-package :rtl-user)
(named-readtables:in-readtable rutils-readtable)

(defstruct point
  parent)

(defun uf-union (point1 point2)
  (:= (point-parent point1) (or (point-parent point2) 
                                point2)))
(defun uf-find (point)
  (let ((parent (point-parent point)))
    (if parent
        (uf-find parent)
        point)))

(defstruct point
  (parent)
  (size 1))

(defun uf-find (point)
  (let ((parent (point-parent point)))
    (ir parent
        (:= (point-parent point) (uf-find parent))
        point)))

(defun uf-union (point1 point2)
  (with ((root1 (uf-find point1))
         (root2 (uf-find point2))
         (major minor (if (> (point-size root1)
                             (point-size root2))
                          (values root1 root2)
                          (values root2 root1))))
    (:+ (point-size major) (point-size minor))
    (:= (point-parent minor) major)))

(defun uf-disjoint (points)
  (let (roots)
    (dolist (point points)
      (let ((root (uf-find point)))
        (when (member root roots)
          return-from uf-disjoint nil))
      (push root roots)))
  t)
