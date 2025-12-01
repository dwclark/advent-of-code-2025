(defpackage :day-01
  (:use :cl)
  (:import-from :utils :read-day-file)
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

(defun count-zeroes (entries)
  (loop with zeroes = 0
	with at = 50
	for entry in entries
	do (setf at (new-at at entry))
	   (if (zerop at) (incf zeroes))
	finally (return zeroes)))

(defun count-pass-zero (entries)
  (loop with by-zero = 0
	with at = 50
	for entry in entries
	do (multiple-value-bind (pos passes) (new-at at entry)
	     (setf at pos)
	     (incf by-zero passes))
	finally (return by-zero)))
	
(defun part-1 ()
  (count-zeroes (parse-file "01")))

(defun part-2 ()
  (count-pass-zero (parse-file "01")))
