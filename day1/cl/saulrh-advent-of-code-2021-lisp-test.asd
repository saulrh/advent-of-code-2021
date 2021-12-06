(defsystem saulrh-advent-of-code-2021-lisp-test
  :author "Saul Reynolds-Haertle <saulrh@saulrh.com>"
  :license "MIT"
  :description "tests for my AoC stuff"
  :depends-on (:saulrh-advent-of-code-2021-lisp
	       :fiveam)
  :components ((:module "test"
		:serial t
		:components
		((:file "packages")
		 (:file "day1")
		 (:file "main"))))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam :run! 'saulrh-advent-of-code-2021-lisp-test:all-tests)))
