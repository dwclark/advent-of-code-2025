(declaim (optimize (debug 3)))

(defpackage :day-10
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:map-combinations #:ensure-gethash #:curry)
  (:import-from :cl-containers #:basic-queue #:enqueue #:dequeue #:empty-p)
  (:import-from :fare-memoization #:define-memo-function #:*memoized* #:memoize)
  (:import-from :function-cache #:defcached #:clear-cache #:cached-results-count)
  (:export #:part-1 #:part-2))

(in-package :day-10)

(defstruct line goal buttons joltage)
  
(defun read-config-line (str)
  (let ((lights (loop for i from 1 below (position #\] str)
		      collecting (if (char= #\. (aref str i)) :off :on) into lights
		      finally (return (make-array (length lights) :initial-contents lights))))
	(buttons (loop with start-pos = (position #\( str)
		       while start-pos
		       collecting (let* ((end-pos (1+ (position #\) str :start start-pos)))
					 (sub-str (subseq str start-pos end-pos))
					 (for-parse (substitute #\Space #\, sub-str)))
				    (setf start-pos (position #\( str :start end-pos))
				    (read-from-string for-parse))))
	(joltage (read-from-string (concatenate 'string "(" (subseq (substitute #\Space #\, str) (1+ (position #\{ str)) (position #\} str)) ")"))))
    (make-line :goal lights :buttons buttons :joltage joltage)))

(defun read-config (day)
  (loop for config-line in (read-day-file day)
	collecting (read-config-line config-line)))

(defun solve-1 (it)
  (let* ((already (make-hash-table :test 'equalp))
	 (start (cons 0 (make-array (length (line-goal it)) :initial-element :off)))
	 (q (make-instance 'basic-queue)))

    (enqueue q start)
    (loop while (not (empty-p q))
	  for (steps . next) = (dequeue q) then (dequeue q)
	  do (if (equalp next (line-goal it))
		 (return steps)
		 (loop for button in (line-buttons it)
		       do (let ((key (cons next button)))
			    (when (not (gethash key already))
			      (setf (gethash key already) t)
			      (enqueue q (cons (1+ steps)
					       (loop with proposed = (copy-seq next)
						     for num in button
						     do (setf (aref proposed num)
							      (if (eq :on (aref proposed num)) :off :on))
						     finally (return proposed)))))))))))

(defun part-1 ()
  (reduce #'+ (mapcar #'solve-1 (read-config "10"))))

(defparameter *press-combinations* nil)

(defun press-combinations (buttons joltages)
  (let ((normalized (loop for button in buttons
			  collecting (loop for i from 0 below (length joltages)
					   collecting (if (member i button :test #'eql) 1 0)))))
    (let ((ret nil))
      (dotimes (n (length normalized))
	(map-combinations #'(lambda (lists)
			      (push (cons (apply #'mapcar #'+ lists) (1+ n)) ret))
			  normalized
			  :length (1+ n)))
      (append (list (cons (make-sequence 'list (length joltages) :initial-element 0) 0)) (reverse ret)))))

(defun solved-p (goal)
  (every #'zerop goal))

(defcached solve-2 (goal)
  (if (solved-p goal) (return-from solve-2 0))
  
  (loop with best-answer = most-positive-fixnum
	for (presses . cost) in *press-combinations*
	do (if (every #'identity (mapcar #'(lambda (i j) (and (<= i j) (= (rem i 2) (rem j 2)))) presses goal))
	       (let ((next-goal (mapcar #'(lambda (i j) (truncate (- j i) 2)) presses goal)))
		 (setf best-answer (min best-answer (+ cost (* 2 (solve-2 next-goal)))))))
	finally (return best-answer)))

(defun part-2 ()
  (let ((lines (read-config "10")))
    (reduce #'+ (loop for line in lines
		      collecting (let ((*press-combinations* (press-combinations (line-buttons line) (line-joltage line))))
				   (clear-cache *solve-2-cache*)
				   (let ((solution (solve-2 (line-joltage line))))
				     (format t "solution: ~A, hash-table count: ~A~%" solution (cached-results-count *solve-2-cache*))
				     solution))))))
