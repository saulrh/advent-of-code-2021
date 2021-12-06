(in-package :saulrh-advent-of-code-2021-lisp-test)

(def-suite all-tests :description "master test suite")
(in-suite all-tests)

(defun test-all ()
  (run! 'day1-tests))
