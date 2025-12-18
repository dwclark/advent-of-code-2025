(declaim (optimize (debug 3)))

(defpackage :day-08
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:curry #:hash-table-keys)
  (:export #:part-1 #:part-2))

(in-package :day-08)

(defun load-data (day)
  (flet ((transform (s)
	   (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")"))))
    (mapcar #'transform (read-day-file day))))

(defun distance (l1 l2)
  (flet ((part (p1 p2)
	   (expt (- p1 p2) 2)))
    (sqrt (reduce #'+ (mapcar #'part l1 l2)))))

(defstruct metric
  (one nil :type cons)
  (two nil :type cons)
  (distance 0d0 :type float))

(defun new-metric (one two)
  (make-metric :one one :two two :distance (distance one two)))

(defun make-metrics (data)
  (loop with ret = nil
	for all on data
	do (if (rest all)
	       (loop with one = (first all)
		     for two in (rest all)
		     do (push (new-metric one two) ret)))
	finally (return (sort ret #'< :key #'metric-distance))))

(defun overlap-p (c1 c2)
  (if (eq c1 c2)
      nil
      (loop for junction being the hash-keys in c1
	    do (if (gethash junction c2)
		   (return t))
	    finally (return nil))))

(defun how-p (metric circuits)
  (loop with one = (metric-one metric)
	with two = (metric-two metric)
	for circuit in circuits
	do (let ((one-in (gethash one circuit))
		 (two-in (gethash two circuit)))
	     (cond ((and one-in two-in)
		    (return (values :noaction circuit)))
		   ((or one-in two-in)
		    (return (values :join circuit)))))
	finally (return (values :new nil))))

(defun merge-circuits (new circuits)
  (let ((merge-to (find-if (curry #'overlap-p new) circuits)))
    (if merge-to
      (loop for junction being the hash-keys in new
	    do (setf (gethash junction merge-to) t)
	    finally (return (merge-circuits merge-to (remove-if (curry #'eq new) circuits))))
      circuits)))

(defun remove-already-in (metrics circuits)
  (flet ((already (metric)
	   (loop for circuit in circuits
		 do (if (and (gethash (metric-one metric) circuit)
			     (gethash (metric-two metric) circuit))
			(return t))
		 finally (return nil))))
    (remove-if #'already metrics)))

(defun make-circuit (metric)
  (let ((circuit (make-hash-table :test 'eq)))
    (setf (gethash (metric-one metric) circuit) t
	  (gethash (metric-two metric) circuit) t)
    circuit))

(defun part-1 ()
  (let* ((metrics (make-metrics (load-data "08")))
	 (subset (subseq metrics 0 1000))
	 (circuits nil))
    (loop while subset
	  do (let ((new-circuit (make-circuit (first subset))))
	       (push new-circuit circuits)
	       (setf subset (rest subset))
	       (setf circuits (merge-circuits new-circuit circuits))
	       (setf subset (remove-already-in subset circuits))))
    (let ((sorted (sort circuits #'> :key #'hash-table-count)))
      (* (hash-table-count (first sorted))
	 (hash-table-count (second sorted))
	 (hash-table-count (third sorted))))))

(defun part-2 ()
  (let* ((metrics (make-metrics (load-data "08")))
	 (circuits nil)
	 (index 0)
	 (last nil))
    (loop for metric in metrics
	  do (multiple-value-bind (how circuit) (how-p metric circuits)
	       (case how
		 (:noaction nil)
		 (:new (push (make-circuit metric) circuits))
		 (:join
		  (setf last metric)
		  (setf (gethash (metric-one metric) circuit) t
			(gethash (metric-two metric) circuit) t)
		  (setf circuits (merge-circuits circuit circuits))))))
    (* (first (metric-one last)) (first (metric-two last)))))
