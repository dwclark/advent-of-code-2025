(defpackage :day-06
  (:use #:cl)
  (:import-from :utils #:read-day-file #:read-lists-of-symbols)
  (:import-from :alexandria #:curry)
  (:export #:part-1 #:part-2))

(in-package :day-06)

(defun read-1 (day)
  (mapcar (curry #'concatenate 'vector) (read-lists-of-symbols day)))

(defun problems-1 (vectors)
  (loop for i from 0 below (length (first vectors))
	collecting (loop for vec in vectors
			 collecting (aref vec i) into problem
			 finally (return (reverse problem)))))

(defun part-1 ()
  (let ((problems (problems-1 (read-1 "06"))))
    (reduce #'+ (mapcar (lambda (p)
			  (apply (first p) (rest p))) problems))))

(defun read-2 (day)
  (let* ((reversed (reverse (read-day-file day)))
	 (ops (first reversed))
	 (numbers (reverse (rest reversed))))
    (values ops numbers)))

(defun ops-columns (str)
  (flet ((find-op (c)
	   (case c
	     (#\+ #'+)
	     (#\* #'*)
	     (otherwise nil))))
    (loop with ret = nil
	  for column from 0 below (length str)
	  do (let ((op (find-op (aref str column))))
	       (if op
		   (push (cons op column) ret)))
	  finally (return (reverse ret)))))

(defun aligned-numbers (ops-cols str)
  (loop for next on ops-cols
	collecting (let ((f (first next))
			 (r (rest next)))
		     (if r
			 (subseq str (cdr f) (1- (cdr (first r))))
			 (subseq str (cdr f)))) into numbers
	finally (return numbers)))

(defun part-2-math (ops-cols aligned)
  (flet ((solve (op-col &rest strs)
	   (let ((op (car op-col))
		 (width (length (first strs))))
	     (loop for index from (1- width) downto 0
		   collecting (loop for str in strs
				    collecting (aref str index) into number
				    finally (return (parse-integer (concatenate 'string number) :junk-allowed t))) into numbers
		   finally (return (apply op numbers))))))
	     
    (reduce #'+ (apply #'mapcar #'solve (append (list ops-cols) aligned)))))

(defun part-2 ()
  (multiple-value-bind (ops numbers) (read-2 "06")
    (let* ((ops-cols (ops-columns ops))
	   (aligned (mapcar (curry #'aligned-numbers ops-cols) numbers)))
      (part-2-math ops-cols aligned))))
