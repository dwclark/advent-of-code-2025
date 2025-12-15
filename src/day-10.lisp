(declaim (optimize (debug 3)))

(defpackage :day-10
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:map-combinations #:ensure-gethash)
  (:import-from :cl-containers #:basic-queue #:enqueue #:dequeue #:empty-p)
  (:export #:part-1 #:part-2))

(in-package :day-10)

(defstruct line goal buttons)
  
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
				    (read-from-string for-parse)))))
    (make-line :goal lights :buttons buttons)))

(defun read-config (day)
  (loop for config-line in (read-day-file day)
	collecting (read-config-line config-line)))

(defun solve-it (it)
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
  (reduce #'+ (mapcar #'solve-it (read-config "10"))))
	       
