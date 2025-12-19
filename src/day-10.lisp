(declaim (optimize (debug 3)))

(defpackage :day-10
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:map-combinations)
  (:import-from :cl-containers #:basic-queue #:enqueue #:dequeue #:empty-p)
  (:import-from :function-cache #:defcached #:clear-cache #:cached-results-count)
  (:import-from :linear-programming #:parse-linear-problem #:solve-problem #:solution-variable)
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

;; converts the button press combinations into variables.
;; for example if the joltages are this: {11, 14, 5, 6, 9}
;; then (2) -> (0,0,1,0,0), (0,3,4) -> (1,0,0,1,1), etc.
;; also combines them into combinations of presses up to pressing each button once
;; so if we were to press (2) & (0,3,4), the combination becomes: (1,0,1,1,1)

;; Why should be use _these_ combinations? Basically because they will give the
;; right answer and hand-waving. In short: guessing.
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

;; the basic idea is to loop through the combinations, only trying
;; the combinations that are "interesting." They are "interesting" because
;; they drastically reduce the search space and they give the right answer.
;; In short: guessing and hand-waving.
(defcached solve-2 (goal)
  (if (solved-p goal) (return-from solve-2 0))
  
  (loop with best-answer = most-positive-fixnum
	for (presses . cost) in *press-combinations*
	;; this line looks for operations where the proposed button press would turn all
	;; off -> on and all on -> off. We can then cut the search space in half because the
	;; number of presses going off -> on and on -> off is 2x the number of presses going
	;; from off -> on -> off and on- > off -> on. This line decides if we can skip that middle
	;; step, thereby effectively keeping the lights the same,
	do (if (every #'identity (mapcar #'(lambda (i j) (and (<= i j) (= (rem i 2) (rem j 2)))) presses goal))
	       ;; do the actual transition tested for in the previous line
	       ;; this is also why we have to include a "no-op" in the press-combinations
	       ;; so we can do a transition _without_ doing any light flipping.
	       ;; This also magically handles the final case where all lights are flipped
	       ;; from the off position to at leas some being on in such a way that will
	       ;; make the rest of the algorithm work correctly.
	       ;; In short: more guessing, more hand-waving. I don't know how this is proved
	       ;; to be true. Again, I suspect that the data were crafted in this way so that
	       ;; this guess works magically.
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

(defun joltage-coefficients (buttons joltages)
  (loop for i from 0 below (length joltages)
	collecting (loop with coefficients = (make-array (length buttons) :initial-element 0)
			 for button in buttons
			 for button-index = 0 then (1+ button-index)
			 do (loop for element in button
				  do (if (= element i)
					 (setf (aref coefficients button-index) 1)))
			 finally (return coefficients))))

(defun solve-lp (list-coefficients joltages)
  (let* ((syms (loop for i from 0 below (length (first list-coefficients))
		     collecting (intern (format nil "X~A" i))))
	 (equation `(min (= w (+ ,@syms))))
	 (gt-zero-constraints (loop for sym in syms collecting `(<= 0 ,sym)))
	 (reduced-syms (loop for coefficients in list-coefficients
			     collecting (loop with ret = nil
					      for i from 0 below (length coefficients)
					      do (if (= 1 (aref coefficients i))
						     (push (nth i syms) ret))
					      finally (return (reverse ret)))))
	 (joltage-constraints (loop for joltage in joltages
				    for reduced in reduced-syms
				    collecting `(= ,joltage (+ ,@reduced))))
	 (constraints (append gt-zero-constraints joltage-constraints))
	 (problem (parse-linear-problem equation constraints))
	 (solution (solve-problem problem)))
    (format t "equation: ~A~%" equation)
    (format t "constraints: ~A~%" constraints)
    (format t "answer: ~A~%" (solution-variable solution 'w))
    (format t "here~%")))
    
	   
(defun part-2-lp ()
  (let ((lines (read-config "10a")))
    (loop for line in lines
	  do (let ((list-coefficients (joltage-coefficients (line-buttons line) (line-joltage line))))
	       (solve-lp list-coefficients (line-joltage line))))))
