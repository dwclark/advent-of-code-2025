(defpackage :day-01
  (:use :cl)
  (:import-from :utils #:read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-01)

(defun parse-file (day)
  (flet ((parse (line)
	   (cons (if (char= #\L (aref line 0)) #'1- #'1+)
		 (parse-integer line :start 1))))
    
    (mapcar #'parse (read-day-file day))))

(defun new-at (at entry)
  (let ((op (car entry))
	(passes 0)
	(pos at))
    (dotimes (i (cdr entry))
      (setf pos
	    (let ((tmp (funcall op pos)))
	      (cond ((= -1 tmp) 99)
		    ((= 100 tmp) 0)
		    (t tmp))))
      (if (zerop pos)
	  (incf passes)))
    (values pos passes)))

(defun counts (entries)
  (loop with at = 50
	with total-at-zero = 0
	with total-passes = 0
	for entry in entries
	do (multiple-value-bind (new-at new-passes) (new-at at entry)
	     (setf at new-at
		   total-at-zero (+ total-at-zero (if (zerop at) 1 0))
		   total-passes (+ total-passes new-passes)))
	finally (return (values total-at-zero total-passes))))
	
(defun part-1 ()
  (multiple-value-bind (one two) (counts (parse-file "01"))
    (declare (ignore two))
    one))

(defun part-2 ()
  (multiple-value-bind (one two) (counts (parse-file "01"))
    (declare (ignore one))
    two))
