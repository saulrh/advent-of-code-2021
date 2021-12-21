(uiop:define-package :day1/source/part1
  (:use :cl)
  (:export :solve-part1))

(in-package :day1/source/part1)

(defun solve-part1 (data)
  (loop for (first second) on data
        while second
        sum (if (< first second) 1 0)))
