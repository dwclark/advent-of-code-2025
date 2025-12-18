(declaim (optimize (debug 3)))

(defpackage :day-11
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :function-cache #:defcached #:clear-cache)
  (:export #:part-1 #:part-2))

(in-package :day-11)

(defun read-data (day)
  (loop for line in (read-day-file day)
	collecting  (read-from-string (concatenate 'string "(" (substitute #\Space #\: line) ")"))))

(defparameter *paths* nil)

(defun count-paths (sym)
  (if (eq sym 'out)
      1
      (let ((path (find-if #'(lambda (lst) (eq sym (first lst))) *paths*)))
	(loop for new-sym in (rest path)
	      summing (count-paths new-sym)))))

(defcached count-paths-2 (sym saw-fft saw-dac)
  (let* ((path (find-if #'(lambda (lst) (eq sym (first lst))) *paths*)))
    (if path
	(reduce #'+ (mapcar (lambda (next-sym)
			      (count-paths-2 next-sym
					     (if saw-fft saw-fft (eq sym 'fft))
					     (if saw-dac saw-dac (eq sym 'dac))))
			    (rest path)))
	(if (and saw-fft saw-dac (eq sym 'out))
	    1
	    0))))
  
(defun part-1 ()
  (let ((*paths* (read-data "11")))
    (count-paths 'you)))

(defun part-2 ()
  (let ((*paths* (read-data "11")))
    (clear-cache *count-paths-2-cache*)
    (count-paths-2 'svr nil nil)))
