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

(defun bin-search-imp (val vec &key (less '<) (test '=) (key 'identity))
  (when (plusp (length vec))
    (let ((beg 0)
	  (end (1- (length vec))))
      (do ()
	  ((= beg end))
	(let ((mid (+ beg (floor (- end beg) 2))))
	  (if (call less (call key (aref vec mid)) val)
	      (:= beg (1+ mid))
	      (:= end mid))))
      (values (aref vec beg)
	      beg
	      (call test (call key (aref vec beg)) val)))))

(defun bin-search-simp (val vec)
  (when (plusp (length vec))
    (let ((beg 0)
	  (end (1- (length vec))))
      (do ()
	  ((= beg end))
	(let ((mid (+ beg (floor (- end beg) 2))))
	  (if (< (aref vec mid) val)
	      (:= beg (1+ mid))
	      (:= end mid))))
      (values (aref vec beg)
	      beg
	      (= (aref vec beg) val)))))

;;; compare find v bin-search-imp
(with ((size 100000000)
       (mid (1+ (/ size 2)))
       (vec (make-array size)))
  (dotimes (i size)
	  (:= (? vec i) i))
  (time (find mid vec))
  (time (bin-search-imp mid vec)))

(defun bogosort (vec comp)
  (dolist (variant (all-permutations vec))
    (dotimes (i (1- (length variant)) (return-from bogosort variant))
      (when (call comp (? variant (1+ i)) (? variant i))
	(return)))))


;;; (/ (* n (- n 1)) 2)
;;; why div by 2? -- for average case?
;;; its called this because for each i
;;; it selects the "best" value to the
;;; which often means smallest
(defun selection-sort (vec comp)
  (dotimes (i (1- (length vec)))	; for every element
    (let ((best (aref vec i)) 		; initialize a "best val" to i
	  (idx i))			; intialize the idx of best val
      (dotimes (j (- (length vec) 1 i))	; from the best val to the right of the array
	(when (call comp (aref vec (+ i 1 j)) best) ; compare the best val and the val
	  (:= best (aref vec (+ i 1 j))	;if this val is "better" update best
	      idx (+ i j 1))))		;update idx of best
      (rotatef (aref vec i) (aref vec idx)))) ;rotate the current value of i (elem were comparing.) with best., if i is in fact the best nothing happens because i is initlized to best
  vec)					;return sorted vector

;;; (/ (* n (- n 1) 2) or (- n 1)
;;; (/ (* n (- n 1)) 4)
(defun insertion-sort (vec comp)
  (dotimes (i (1- (length vec))) ; for each element 
    (do ((j i (1- j))) ; start at the current value and go back through the left portion
	((minusp j)) ; once we go past 0 end loop
      (if (call comp (aref vec (1+ j)) (aref vec j)) ; starting with the first right value, check to see if its better
	  (rotatef (aref vec (1+ j)) (aref vec j)) ; if its better rotate the two, now make sure its properly sorted by comparing the new value with the one before it.
	  (return)))) ; if not break the inner loop
  vec)

;;; pi hovers behind and keeps up with values that are being
;;; sorted based on the pivot, if a value is encountered that is = or greater
;;; then the pi stays put until another lower than pivot value is found, which
;;; then gets swapped w/ the pi. wherever pi lands at the end of the array is
;;; either greater or equal to the pivot. so we swap that end pivot with the
;;; pi post dotimes loop, then we recursively call up to pivot i (which is
;;; for sure sorted based on the end pivot), this represents the left half
;;; and we also recursively call on the right half of the array.
;;; repeat and sort the array
(defun quicksort (vec comp)
  (when (> (length vec) 1) ; if we have more than 1 thing
    (with ((pivot-i 0) ; create a pivoti starting at 0.
	   (pivot (aref vec (1- (length vec))))) ; select start pivot at the end
      (dotimes (i (1- (length vec))) ; for each item in vec
	(when (call comp (aref vec i) pivot) ; when the current value is better than pivot
	  (rotatef (aref vec i) ; swap the current value with the pivoti
		    (aref vec pivot-i))
	  (:+ pivot-i))) ; otherwise increment pivot-i
      (rotatef (aref vec (1- (length vec))) ; at the end of one run swap pivoti and value at end of the array
	       (aref vec pivot-i))
      (quicksort (slice vec 0 pivot-i) comp) ; recurse on the left half of the array,
      (quicksort (slice vec (1+ pivot-i)) comp))) ; recurse on the right half of the array
  vec)

(defun random-vec (size)
  (let ((vec (make-array size)))
    (dotimes (i size)
      (:= (? vec i) (random size)))
    vec))

(defun print-sort-timings (sort-name sort-fn vec)
  (let ((vec (copy-seq vec))
	(len (length vec)))
    (format t "= ~Asort of random vector (length=~A) =~%"
	    sort-name len)
    (time (call sort-fn vec '<))
    (format t "= ~Asort of sorted vector (length=~A) =~%"
	    sort-name len)
    (time (call sort-fn vec '<))
    (format t "= ~Asort of reverse sorted vector (length=~A) =%"
	    sort-name len)
    (time (call sort-fn vec '>))
    nil))
