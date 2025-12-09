(declaim (optimize (debug 3)))

(defpackage :day-08
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:curry #:hash-table-keys)
  (:import-from :fare-memoization #:define-memo-function)
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

(defun total-connections (data)
  (loop for i from (length data) downto 1
	summing (- i 1)))

(defun make-distances (data)
  (loop with distances = (make-hash-table)
	for l1 on data
	do (if (rest l1)
	       (loop with one = (first l1)
		     for two in (rest l1)
		     do (setf (gethash (distance one two) distances) (cons one two))))
	finally (return (values (sort (hash-table-keys distances) #'<) distances))))

(defun make-circuits (distances table)
  (let ((circuits nil))
    (labels ((add (one two)
	       (let ((found (find-if (lambda (circuit)
				       (or (gethash one circuit)
					   (gethash two circuit))) circuits)))
		 (if found
		     (progn
		       (setf (gethash one found) t
			     (gethash two found) t)
		       found)
		     (let ((new-circuit (make-hash-table :test 'equal)))
		       (setf (gethash one new-circuit) t
			     (gethash two new-circuit) t)
		       (push new-circuit circuits)
		       new-circuit))))

	     (overlaps-p (m1 m2)
	       (when (not (eq m1 m2))
		 (loop for key being the hash-keys in m1
		       do (if (gethash key m2)
			      (return t))
		       finally (return nil))))
	     
	     (join-circuits (circuit)
	       (let ((overlap (find-if (curry #'overlaps-p circuit) circuits)))
		 (when overlap
		   (loop for key being the hash-keys in circuit
			 do (setf (gethash key overlap) t))
		   (setf circuits (remove-if (curry #'eq circuit) circuits))
		   (join-circuits overlap)))))

      (dolist (distance distances)
	(destructuring-bind (one . two) (gethash distance table)
	  (let ((circuit (add one two)))
	    (join-circuits circuit))))

      (sort circuits #'> :key #'hash-table-count))))

(defun part-1 ()
  (multiple-value-bind (distances table) (make-distances (load-data "08"))
    (flet ((subset-of (distances)
	     (loop with new-table = (make-hash-table)
		   for distance in distances
		   do (setf (gethash distance new-table) (gethash distance table))
		   finally (return new-table))))
      (let* ((num 1000)
	     (shortest-n (subseq distances 0 num))
	     (circuits (make-circuits shortest-n (subset-of shortest-n))))
      (* (hash-table-count (first circuits))
	 (hash-table-count (second circuits))
	 (hash-table-count (third circuits)))))))

#|(defun part-2 ()
  (multiple-value-bind (distances table) (make-distances (load-data "08a"))
    (join-others (circuit)
	       (loop with changed = t
		     while changed
		     do (loop for distance being the hash-keys in table
			      do (destructuring-bind (one . two) (gethash distance table)
				   (when (or (gethash one circuit)
					     (gethash two circuit))
	
    (loop for distance in distances
	  do (format t "~A~%" (gethash distance table)))))

|#
