(declaim (optimize (debug 3)))

(defpackage :day-12
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :function-cache #:defcached #:clear-cache)
  (:export #:part-1 #:part-2))

(in-package :day-12)

(defun parse-data (day)
  (let ((contents (read-day-file day))
	(pieces nil))
    (labels ((read-piece (lst)
	       (loop with table = (make-hash-table :test #'equal)
		     for i from 0 to 4
		     for sub on lst
		     do (let ((line (first sub)))
			  (if (< 0 i 4)
			      (loop for col from 0 below (length line)
				    do (if (char= #\# (aref line col))
					   (setf (gethash (cons col (1- i)) table) t)))))
		     finally (push table pieces)
			     (return (rest sub))))

	     (read-pieces ()
	       (let ((next contents))
		 (dotimes (n 6)
		   (setf next (read-piece next)))
		 (setf pieces (reverse pieces))
		 next))

	     (read-region (line)
	       (let* ((just-spaces (substitute #\Space #\: (substitute #\Space #\x line)))
		      (for-read (concatenate 'string "(" just-spaces ")")))
		 (read-from-string for-read)))

	     (read-regions (lines)
	       (loop for line in lines
		     collecting (read-region line))))
      
      (let ((start-regions (read-pieces)))
	(values pieces (read-regions start-regions))))))

(defun total-units (region)
  (* (first region) (second region)))

(defun total-tiles (region)
  (* (truncate (first region) 3) (truncate (second region) 3)))

(defun min-units-needed (region pieces)
  (loop for piece in pieces
	for count in (rest (rest region))
	summing (* count (hash-table-count piece))))

(defun lazy-tiles-needed (region)
  (loop for count in (rest (rest region))
	summing count))

(defun part-1 ()
  (multiple-value-bind (pieces regions) (parse-data "12")
    (loop for region in regions
	  summing (if (<= (lazy-tiles-needed region) (total-tiles region)) 1 0) into total
	  do (assert (or (< (total-units region) (min-units-needed region pieces))
			 (<= (lazy-tiles-needed region) (total-tiles region))))
	  finally (return total))))
