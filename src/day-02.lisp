(defpackage :day-02
  (:use #:cl #:fsio-csv #:fsio-utils)
  (:import-from :utils :day-file-path)
  (:import-from :alexandria :hash-table-keys)
  (:export #:part-1 #:part-2))

(in-package :day-02)

(mixin stm-csv->table stm-parser stm-functions table table-accum)

(defun read-contents (day)
  (with-open-file (stm (day-file-path day))
    (let* ((tab (table-new :has-headers nil :keep-rows t))
	   (parser (stm-parser-new stm))
	   (tab (stm-csv->table parser tab)))
      (loop for item across (table-get-row tab 0)
	    collecting ((lambda ()
			  (let ((pos (position #\- item)))
			    (list (subseq item 0 pos) (subseq item (1+ pos))))))))))

(defun lower-sub-bound (sequence-length)
  (parse-integer (format nil "~A~v@{~A~:*~}" "1" (1- sequence-length) "0")))

(defun upper-sub-bound (sequence-length)
  (parse-integer (format nil "~v@{~A~:*~}" sequence-length "9")))

(defun to-pattern (sub repeat)
  (parse-integer (format nil "~v@{~A~:*~}" repeat sub)))

(defun invalids-for-length (list-pair sub-length)
  (let ((lengths (remove-duplicates (list (length (first list-pair)) (length (second list-pair)))))
	(lower (parse-integer (first list-pair)))
	(upper (parse-integer (second list-pair))))
    (loop with all = (make-hash-table)
	  for len in lengths
	  do (if (and (zerop (rem len sub-length)) (<= 2 (/ len sub-length)))
		 (loop with repeat = (/ len sub-length)
		       for sub from (lower-sub-bound sub-length) upto (upper-sub-bound sub-length)
		       do (let ((num (to-pattern sub repeat)))
			    (if (<= lower num upper)
				(setf (gethash num all) t)))))
	  finally (return (hash-table-keys all)))))

(defun part-1 ()
  (flet ((invalids-for (list-pair)
	   (loop with done = nil
		 with accum = (make-hash-table)
		 for str in list-pair
		 for str-length = (length str) then (length str)
		 do (let ((evenly (zerop (rem str-length 2)))
			  (sub-length (/ str-length 2)))
		      (when (and evenly (not (member str-length done :test #'=)))
			(push str-length done)
			(dolist (val (invalids-for-length list-pair sub-length))
			  (setf (gethash val accum) t))))
		 finally (return (hash-table-keys accum)))))
    
    (reduce #'+ (mapcan #'invalids-for (read-contents "02")))))

(defun part-2 ()
  (flet ((invalids-for (list-pair)
	   (loop with done = nil
		 with accum = (make-hash-table)
		 for str in list-pair
		 for str-length = (length str) then (length str)
		 do (when (not (member str-length done :test #'=))
		      (push str-length done)
		      (loop for sub-length from 1 upto (truncate str-length 2)
			    do (dolist (val (invalids-for-length list-pair sub-length))
				 (setf (gethash val accum) t))))
		 finally (return (hash-table-keys accum)))))
    
    (reduce #'+ (mapcan #'invalids-for (read-contents "02")))))
