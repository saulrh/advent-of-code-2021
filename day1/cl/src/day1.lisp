(in-package :saulrh-advent-of-code-2021-lisp.day1)

(defun part1 (data)
  (loop for (first second) on data
	while second
	sum (if (< first second) 1 0)))

(defun part2 (data)
  (let ((window-sums (loop for (first second third) on data
			   while third
			   collect (+ first second third))))
    (part1 window-sums)))

