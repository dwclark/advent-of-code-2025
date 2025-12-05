(defpackage :day-05
  (:use #:cl)
  (:import-from :utils #:read-day-file #:range-overlap-p)
  (:import-from :alexandria #:curry)
  (:export #:part-1 #:part-2))

(in-package :day-05)

(defun ranges-and-ids (day)
  (let ((lines (read-day-file day))
	(ranges nil)
	(ids nil))
    (loop for line in lines
	  do (cond
	       ((find #\- line)
		(let ((pos (position #\- line)))
		  (push (cons (parse-integer line :start 0 :end pos)
			      (parse-integer line :start (1+ pos))) ranges)))
	       ((not (zerop (length line)))
		(push (parse-integer line) ids))))
    (values (reverse ranges) (reverse ids))))

(defun part-1 ()
  (multiple-value-bind (ranges ids) (ranges-and-ids "05")
    (labels ((in-range (id r) (<= (car r) id (cdr r)))
	     (fresh-p (id)
	       (if (find-if (curry #'in-range id) ranges)
		   1 0)))
      (reduce #'+ (mapcar #'fresh-p ids)))))

(defun range-amount (r)
  (if (<= (car r) (cdr r))
      (1+ (- (cdr r) (car r)))
      0))

(defun range-valid-p (r)
  (not (zerop (range-amount r))))

(defun uniqueify (orig ranges)
  (loop for range in ranges
	while (and (not (eq orig range)) (range-valid-p orig))
	do (if (and (range-valid-p range) (range-overlap-p range orig))
	       (setf (car orig) (1+ (cdr range))))))

(defun part-2 ()
  (let* ((ranges (ranges-and-ids "05"))
	 (ranges (sort ranges #'< :key #'car)))
    (dolist (range ranges)
      (uniqueify range ranges))
    (reduce #'+ (mapcar #'range-amount ranges))))
