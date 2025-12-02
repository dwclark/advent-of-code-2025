(defpackage :day-02
  (:use #:cl #:fsio-csv #:fsio-utils)
  (:import-from :utils :day-file-path)
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
			    (cons (subseq item 0 pos) (subseq item (1+ pos))))))))))

(defun find-invalid (pair)
  (let* ((lower (parse-integer (car pair)))
	 (upper (parse-integer (cdr pair)))
	 (tmp (car pair))
	 (sub-length (if (= 1 (length tmp)) 1 (floor (/ (length tmp) 2))))
	 (sub (parse-integer (subseq tmp 0 sub-length))))
    
    (loop with accum = nil
	  for num = sub then (1+ num)
	  for str = (format nil "~A~A" num num) then (format nil "~A~A" num num)
	  for to-test = (parse-integer str) then (parse-integer str)
	  while (<= to-test upper)
	  do (if (<= lower to-test)
		 (push to-test accum))
	  finally (return (reverse accum)))))

(defun part-1 ()
  (reduce #'+ (mapcan #'find-invalid (read-contents "02"))))
