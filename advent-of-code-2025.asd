(defsystem #:advent-of-code-2025
  :description "Advent of Code 2025"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("alexandria" "cl-ppcre" "array-operations" "infix-math" "cl-heap"
                            "cl-containers" "split-sequence" "function-cache")
  :serial t
  :components ((:file "src/utils")
               (:file "src/day-01")
               (:file "src/day-02")
               (:file "src/day-03")
               (:file "src/day-04")
               (:file "src/day-05")
               (:file "src/day-06")
               (:file "src/day-07")
               (:file "src/day-08")
               (:file "src/day-09")
               (:file "src/day-10")
               (:file "src/day-11")
               (:file "src/day-12")))
