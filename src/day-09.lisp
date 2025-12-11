(declaim (optimize (debug 3)))

(defpackage :day-09
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:curry #:hash-table-keys)
  (:export #:part-1 #:part-2))

(in-package :day-09)

(defstruct (point (:conc-name nil))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct shape
  (area 0 :type fixnum))

(defstruct (line (:include shape))
  (at 0 :type fixnum)
  (direction nil :type symbol)
  (lower 0 :type fixnum)
  (upper 0 :type fixnum))

(defstruct (rectangle (:include shape))
  (ul nil :type point)
  (lr nil :type point))

(defun find-corners (p1 p2)
  (let ((left-x (min (x p1) (x p2)))
	(right-x (max (x p1) (x p2)))
	(upper-y (min (y p1) (y p2)))
	(lower-y (max (y p1) (y p2))))
    (values (make-point :x left-x :y upper-y)
	    (make-point :x right-x :y lower-y))))

(defun make-shape (p1 p2)
  (cond ((= (x p1) (x p2))
	 (let ((lower (min (y p1) (y p2)))
	       (upper (max (y p1) (y p2))))
	   (make-line :direction :vertical :at (x p1) :lower lower :upper upper :area (1+ (- upper lower)))))
	((= (y p1) (y p2))
	 (let ((lower (min (x p1) (x p2)))
	       (upper (max (x p1) (x p2))))
	   (make-line :direction :horizontal :at (y p1) :lower lower :upper upper :area (1+ (- upper lower)))))
	(t
	 (multiple-value-bind (ul lr) (find-corners p1 p2)
	   (make-rectangle :ul ul :lr lr :area (* (1+ (- (x lr) (x ul)))
						  (1+ (- (y lr) (y ul)))))))))
(defun load-data (day)
  (flet ((transform (s)
	   (let ((lst (read-from-string (concatenate 'string "(" (substitute #\Space #\, s) ")"))))
	     (make-point :x (car lst) :y (cadr lst)))))
    (mapcar #'transform (read-day-file day))))

(defun make-shapes (points)
  (loop with vec = (make-array 0 :adjustable t :fill-pointer 0)
	for sub on points
	do (if (rest sub)
	       (loop with p1 = (first sub)
		     for p2 in (rest sub)
		     do (vector-push-extend (make-shape p1 p2) vec)))
	finally (return (sort vec #'> :key #'shape-area))))

(defun part-1 ()
  (let ((shapes (make-shapes (load-data "09"))))
    (shape-area (aref shapes 0))))

(defun categorize (shapes)
  (loop with h-lines = (make-hash-table)
	with v-lines = (make-array 0 :adjustable t :fill-pointer 0)
	with rectangles = (make-array 0 :adjustable t :fill-pointer 0)
	for shape across shapes
	do (cond ((and (typep shape 'line) (eq :horizontal (line-direction shape)))
		  (setf (gethash (line-at shape) h-lines) shape))

		 ((typep shape 'line)
		  (vector-push-extend shape v-lines))

		 (t (vector-push-extend shape rectangles)))
	finally (return (values h-lines (sort v-lines #'< :key #'line-at) rectangles))))

(defun part-2 ()
  (multiple-value-bind (h-lines v-lines rectangles) (categorize (make-shapes (load-data "09")))
    ;;(format t "h-lines: ~A~%" h-lines)
    ;;(format t "v-lines: ~A~%" v-lines)
    (flet ((in-polygon (p)
	     (let ((h-line (gethash (y p) h-lines)))
	       (if (and h-line (<= (line-lower h-line) (x p) (line-upper h-line)))
		   (return-from in-polygon t)))
	     
	     (let ((lowest (position-if (curry #'<= (x p)) v-lines :key #'line-at)))
	       (let ((v-line (aref v-lines lowest)))
		 (if (and (= (x p) (line-at v-line))
			  (<= (line-lower v-line) (y p) (line-upper v-line)))
		     (return-from in-polygon t))
		 (loop with count = 0
		       for idx from lowest below (length v-lines)
		       do (setf v-line (aref v-lines idx))
			  (if (<= (line-lower v-line) (y p) (line-upper v-line))
			      (incf count))
		       finally (return (oddp count)))))))
      (loop named outer
	    for r across rectangles
	    do (format t "~A~%" r)
	       (loop named testing
		     for xval from (x (rectangle-ul r)) to (x (rectangle-lr r))
		     do (loop for yval from (y (rectangle-ul r)) to (y (rectangle-lr r))
			      do (let ((p (make-point :x xval :y yval)))
				   (if (not (in-polygon p))
				       (return-from testing nil))))
		     finally (return-from outer r))))))
