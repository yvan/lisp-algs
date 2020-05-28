(ql:quickload :rutils)
(in-package :rtl-user)
(named-readtables:in-readtable rutils-readtable)

(let ((vec (make-array 3 :initial-contents '(1.0 2.0 3.0))))
  (print (aref vec 0))
  (print (? vec 1))
  (:= (aref vec 2) 4.0)
  (print (? vec 2))
  (aref vec 3))

(let ((vec #(1.0 2.0 3.0)))
  (:= (aref vec 2) nil)
  (print vec))

(with ((vec (vec 1 2 3))
       (part (slice vec 2)))
  (print part)
  (:= (? part 0) 4)
  (print part)
  vec)

(defun map-vec (fn vec)
  (let ((rez (make-array (length vec))))
    (dotimes (i (length vec))
      (:= (aref rez i) (call fn (aref vec i))))
    rez))

(defun clumsy-filter-vec (pred vec)
  (let ((rez (make-array (length vec) :fill-pointer 0)))
    (dotimes (i (length vec))   
      (when (call pred (aref vec i))	
        (vector-push (aref vec i) rez)))
    rez))

(let ((vec (make-array 0 :fill-pointer t :adjustable t)))
  (dotimes (i 10)
    (vector-push-extend i vec)
    (describe vec)))

(defun m* (m1 m2)
  (with ((n (array-dimension m1 1))	;get the cols of mat1
	 (n1 (array-dimension m1 0))	;get the rows of mat1
	 (n2 (array-dimension m2 1))	;get the cols of mat2
	 (rez (make-array (list n1 n2)))) ;make a new array which is mat1nrows x mat2ncols
    (assert (= n (array-dimension m2 0))) ;make sure the cols of m1 and rows of m2 allow matmul
    (dotimes (i n1)			;for every row in mat1
      (dotimes (j n2)			;for every col in mat2
	(let ((cur 0))			;create accumulator 
	  (dotimes (k n)		;for each col in mat1
	    (:+ cur (* (aref m1 i k)	;multiple each col val of mat1 by the row val of mat2 
		       (aref m2 k j))))	;and accumulate the result in cur
	  (:= (aref rez i j) cur))))    ;set a val in result matrix which is a mat1nrows x mat2ncols
    rez))				;return result mat

(let ((vec #v((pair :foo :bar) (pair :baz :quux))))
  (print (find (pair :foo :bar) vec))
  (print (find (pair :foo :bar) vec :test 'equal))
  (print (find (pair :bar :baz) vec :test 'equal))
  (print (find :foo vec :key 'lt))
  (print (find :quux vec :key 'rt)))

(defun bin-search (val vec &optional (pos 0))
  (if (> (length vec) 1)
      (with ((mid (floor (length vec) 2))
	     (cur (aref vec mid)))
	(cond ((< cur val) (bin-search val
				       (slice vec mid)
				       (+ pos mid)))
	      ((> cur val) (bin-search val
				       (slice vec 0 mid)
				       pos))
	      (t (+ pos mid))))
      (when (= (aref vec 0) val)
	pos)))
;;; imagine you have n=100 elements in your arry you want to bin search.
;;; (log n 2) then is the number of comparisons you will need to perform to find an element.
;;; to get the number of comparisons witout the LAST comparison of the bin search we subtract 1 from it giving: (1- (log n 2))
;;; each of these (1- (log n 2)) comparisons has a chance to trigger (t (+ pos mid)) and exit early from bin-search
;;; to find the probability that this happens, we divide but these comparisons by the number of elements, so (/ (1- (log n 2)) n), in other words, the number of times we have the possibility of exiting early divided by the number of elements (things that could be searched) is the maximum chance we have at an early exit
;;; so now we have the probability (really max prob.) of an "early exit" from our function. 1 - early exit prob, is the probability that we will NOT exit early, i.e. the probability that we will have to go through all (log n 2) comparisons, so this gives us (- 1 (/ (1- (log n 2)) n))
;;; this function asymptotically approaches 1. the bigger n gets (n=10, prob=76.7, n=1000, prob=99.1)which means the probability of having to go through all comparisons gets bigger and bigger, the probability of having an early exit via (t (+ pos mid)) gets smaller and smaller and so that short (t (+ pos mid)) circuit is actually kind of useless.
