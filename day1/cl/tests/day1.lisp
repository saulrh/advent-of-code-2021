(in-package :saulrh-advent-of-code-2021-lisp-test)

(def-suite day1
  :description "tests for day 1")
(in-suite day1)

(setf *SAMPLE-DATA* '(199
                      200
                      208
                      210
                      200
                      207
                      240
                      269
                      260
                      263))

(test test-part1
      (is (= 7 (part1 *SAMPLE-DATA*))))

(test test-part2
      (is (= 5 (part2 *SAMPLE-DATA*))))

