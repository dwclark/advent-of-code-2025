(declaim (optimize (debug 3)))

(defpackage :day-09
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:curry #:hash-table-keys)
  (:export #:part-1 #:part-2))

(in-package :day-09)

(defun load-data (day)
  (flet ((transform (s)
	   (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")"))))
    (mapcar #'transform (read-day-file day))))

(defstruct rectangle
  (one nil :type cons)
  (two nil :type cons)
  (size 0 :type fixnum))

(defun new-rectangle (one two)
  (make-rectangle :one one :two two
		  :size (* (abs (1+ (- (first one) (first two))))
			   (abs (1+ (- (second one) (second two)))))))

(defun make-rectangles (data)
  (loop with ret = (make-array 0 :adjustable t :fill-pointer t)
	for all on data
	do (loop with one = (first all)
		 for two in (rest all)
		 do (vector-push-extend (new-rectangle one two) ret))
	finally (return (sort ret #'> :key #'rectangle-size))))
	       
(defun part-1 ()
  (let ((rectangles (make-rectangles (load-data "09"))))
    (rectangle-size (aref rectangles 0))))
