(declaim (optimize (debug 0)))

(defpackage :day-07
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:curry #:hash-table-keys)
  (:import-from :fare-memoization #:define-memo-function)
  (:export #:part-1 #:part-2))

(in-package :day-07)
 
(defun initial (day)
  (read-day-file day))

(defun count-splits (strs)
  (let ((beams (make-hash-table))
	(splits 0))
    (flet ((new-beams (str)
	     (let ((current-beams (hash-table-keys beams)))
	       (clrhash beams)
	       (loop for beam in current-beams
		     do (if (char= #\^ (aref str beam))
			    (setf (gethash (1- beam) beams) t
				  (gethash (1+ beam) beams) t
				  splits (1+ splits))
			    (setf (gethash beam beams) t))))))
      (setf (gethash (position #\S (first strs) :test #'char=) beams) t)
      (dolist (str (rest strs))
	(new-beams str)))
    splits))

(defparameter *strs* nil)
(defparameter *memoized* nil)

(define-memo-function take-path (beam level)
  (let ((current (if (< level (length *strs*)) (aref *strs* level) nil)))
    (cond ((not current)
	   1)
	  ((char= #\^ (aref current beam))
	   (+ (take-path (1- beam) (1+ level))
	      (take-path (1+ beam) (1+ level))))
	  (t
	   (take-path beam (1+ level))))))

(defun part-1 ()
  (count-splits (initial "07")))

(defun part-2 ()
  (let ((*strs* (map 'vector #'identity (read-day-file "07")))
	(*memoizer* (make-hash-table :test 'equal)))
    (take-path (position #\S (aref *strs* 0) :test #'char=) 1)))
