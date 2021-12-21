(uiop:define-package :day1/main
  (:use :cl)
  (:import-from :day1/source)
  (:import-from :jonathan)
  (:export :solve))

(in-package :day1/main)

(defun solve (input-filename)
  (let ((data (jonathan:parse (uiop:read-file-string input-filename))))
    (format t "~&~D~%" (day1/source:solve-part1 data))
    (format t "~&~D~%" (day1/source:solve-part2 data))))
