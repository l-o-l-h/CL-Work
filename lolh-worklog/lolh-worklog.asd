;;; lolh-worklog.asd
;;; Time-stamp: <2023-01-29 23:09:00 minilolh3>

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
