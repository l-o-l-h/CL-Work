;;; lolh-worklog2.asd
;;; Time-stamp: <2023-02-27 15:42:45 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-23
;;; Version: 0.0.1

;;; Commentary:
;; Creates the system "lolh-worklog2", which uses cl-rbt, Red-Black
;; trees.

;; Test the system with (asdf:test-system "lolh-worklog2")

;;; Code:

(defsystem "lolh-worklog2"
  :description "Procedures operating upon worklogs using cl-rbt."
  :version "0.0.1"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CC0 1.0 Universal"
  :depends-on ("lolh-utils" "local-time" "local-time-duration" "parse-float")
  :components ((:module "src"
		:serial t
		:components ((:file "lolh-worklog2-package")
			     (:file "lolh-worklog2-definitions")
			     (:file "lolh-worklog2-classes")
			     (:file "lolh-worklog2"))))
  :in-order-to ((test-op (test-op "lolh-worklog2/t"))))

(defsystem "lolh-worklog2/t"
  :description "Tests for the lolh-worklog2 system."
  :version "0.0.1"
  :author "LOLH-LINC <lincolnlaw@mac.com"
  :license "CCO 1.0 Universal"
  :depends-on ("fiveam" "lolh-worklog2" "lolh-utils")
  :components ((:module "t"
			:components ((:file "lolh-worklog2-tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :main-worklog2-tests)))

;;; End lolh-worklog2.asd
