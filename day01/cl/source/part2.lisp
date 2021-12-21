(uiop:define-package :day1/source/part2
  (:use :cl)
  (:import-from :day1/source/part1
                :solve-part1)
  (:export :solve-part2))

(in-package :day1/source/part2)

(defun solve-part2 (data)
  (let ((window-sums (loop for (first second third) on data
                           while third
                           collect (+ first second third))))
    (solve-part1 window-sums)))
