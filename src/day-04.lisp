(defpackage :day-04
  (:use #:cl)
  (:import-from :utils #:read-day-file #:*to-add/8*)
  (:import-from :alexandria #:rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-04)

(defun get-grid (day)
  (loop with contents = (read-day-file day)
	with table = (make-hash-table :test 'equal)
	for line in contents
	for row from 0 below (length contents)
	do (loop for c across line
		 for col from 0 below (length line)
		 do (if (char= #\@ c)
			(setf (gethash (cons row col) table) t)))
	finally (return table)))

(defun part-1 ()
  (flet ((cell+ (one two) (cons (+ (car one) (car two)) (+ (cdr one) (cdr two)))))
    (let ((table (get-grid "04"))
	  (total 0))
      (loop for cell being the hash-keys in table
	    do (if (< (reduce #'+ (mapcar #'(lambda (to-add)
					      (let ((new-cell (cell+ to-add cell)))
						(if (gethash new-cell table) 1 0))) *to-add/8*)) 4)
		   (incf total))
	    finally (return total)))))

(defun can-remove (table)
  (flet ((cell+ (one two) (cons (+ (car one) (car two)) (+ (cdr one) (cdr two)))))
    (loop with ret = nil
	  for cell being the hash-keys in table
	  do (if (< (reduce #'+ (mapcar #'(lambda (to-add)
					    (let ((new-cell (cell+ to-add cell)))
					      (if (gethash new-cell table) 1 0))) *to-add/8*)) 4)
		 (push cell ret))
	  finally (return ret))))

(defun part-2 ()
  (let ((table (get-grid "04"))
	(removed 0))
    (loop for to-remove = (can-remove table) then (can-remove table)
	  while to-remove
	  do (incf removed (length to-remove))
	     (dolist (cell to-remove)
	       (remhash cell table)))
    removed))
	

