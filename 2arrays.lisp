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
;;; why div by 2?
(defun selection-sort (vec comp)
  (dotimes (i (1- (length vec)))
    (let ((best (aref vec i))
	  (idx i))
      (dotimes (j (- (length vec) i 1))
	(when (call comp (aref vec (+ i j 1)) best)
	  (:= best (aref vec (+ i j 1))
	      idx (+ i j 1))))
      (rotatef (aref vec i) (aref vec idx))))
  vec)

;;; best: (- n 1)
;;; worst: (/ (* n (- n 1) 2)
(defun insertion-sort (vec comp)
  (dotimes (i (1- (length vec)))
    (do ((j i (1- j)))
	((minusp j))
      (if (call comp (aref vec (1+ j)) (aref vec j))
	  (rotatef (aref vec (1+ j)) (aref vec j))
	  (return))))
  vec)

(defun quicksort (vec comp)
  (when (> (length vec) 1)
    (with ((pivot-i 0)
	   (pivot (aref vec (1- (length vec)))))
      (dotimes (i (1- (length vec)))
	(when (call comp (aref vec i) pivot)
	  (roatatef (aref vec i)
		    (aref vec pivot-i))
	  (:+ pivot-i)))
      (rotatef (aref vec (1- (length vec)))
	       (aref vec pivot-i))
      (quicksort (slice vec 0 pivot-i) comp)
      (quicksort (slice vec (1+ pivot-i)) comp)))
  vec)
