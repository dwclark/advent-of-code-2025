(declaim (optimize (debug 0)))

(defpackage :day-08
  (:use #:cl)
  (:import-from :utils #:read-day-file)
  (:import-from :alexandria #:curry #:hash-table-keys)
  (:import-from :fare-memoization #:define-memo-function)
  (:export #:part-1 #:part-2))

(in-package :day-08)
