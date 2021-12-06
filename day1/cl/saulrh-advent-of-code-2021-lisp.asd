(defsystem saulrh-advent-of-code-2021-lisp
  :author "Saul Reynolds-Haertle <saulrh@saulrh.com>"
  :maintainer "Saul Reynolds-Haertle <saulrh@saulrh.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/saulrh/advent-of-code-2021-lisp"
  :bug-tracker "https://github.com/saulrh/advent-of-code-2021-lisp/issues"
  :source-control (:git "git@github.com:saulrh/advent-of-code-2021-lisp.git")
  :description "My attempt at Advent of Code 2021 using Common Lisp"
  :depends-on ()
  :components ((:module "src"
		:serial t
		:components
		((:file "packages")
		 (:file "util")
		 (:file "day1"))))
  :long-description
  #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op saulrh-advent-of-code-2021-lisp-tests))))
