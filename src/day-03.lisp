(defpackage :day-03
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-03)

(defun find-largest-for-length (vec start num-length)
  (loop with largest = 0
	with largest-index = 0
	for i from start below (- (length vec) (1- num-length))
	do (let ((to-test (aref vec i)))
	     (if (< largest to-test)
		 (setf largest to-test largest-index i)))
	finally (return (values largest largest-index))))

(defun find-largest-number (vec num-length)
  (loop with accum = 0
	with start-index = 0
	for current-length from num-length downto 1
	do (multiple-value-bind (num index) (find-largest-for-length vec start-index current-length)
	     (incf accum (* (expt 10 (1- current-length)) num))
	     (setf start-index (1+ index)))
	finally (return accum)))

(defun max-of-length (len)
  (labels ((split-nums (str)
	     (map 'vector #'(lambda (c) (digit-char-p c)) str)))
    (reduce #'+ (mapcar (rcurry #'find-largest-number len) (mapcar #'split-nums (read-day-file "03"))))))

(defun part-1 ()
  (max-of-length 2))

(defun part-2 ()
  (max-of-length 12))
  
