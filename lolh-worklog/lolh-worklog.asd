;;; lolh-worklog.asd
;;; Time-stamp: <2023-01-31 00:36:14 minilolh3>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version: 0.0.10

;;; Commentary:
;; Creates the system "lolh-worklog", which is tested by the system "lolh-worklog/t".
;; Test the system with (asdf:test-system :lolh-worklog)

;;; Code:

(defsystem "lolh-worklog"
  :description "Procedures operating upon worklogs."
  :version "0.0.10"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CC0 1.0 Universal"
  :depends-on ("lolh-utils" "local-time" "local-time-duration")
  :components ((:module "src"
		:serial t
		:components ((:file "lolh-worklog-package")
			     (:file "lolh-worklog-definitions")
			     (:file "lolh-worklog-classes")
			     (:file "lolh-worklog"))))
  :in-order-to ((test-op (test-op "lolh-worklog/t"))))

(defsystem "lolh-worklog/t"
  :description "Tests for the lolh-worklog system."
  :version "0.0.10"
  :author "LOLH-LINC <lincolnlaw@mac.com"
  :license "CCO 1.0 Universal"
  :depends-on ("fiveam" "lolh-worklog")
  :components ((:module "t"
			:components ((:file "lolh-worklog-tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :root-worklog-tests)))

;;; End lolh-worklog.asd
