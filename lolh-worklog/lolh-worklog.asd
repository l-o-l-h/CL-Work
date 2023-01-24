;;; lolh-worklog.asd
;;; Time-stamp: <2023-01-24 13:35:51 minilolh3>

(defsystem "lolh-worklog"
  :description "Procedures operating upon worklogs."
  :version "0.0.10"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CC0 1.0 Universal"
  :depends-on ("lolh-utils" "local-time" "local-time-duration")
  :pathname "src/"
  :components ((:file "lolh-worklog-package")
	       (:file "lolh-worklog-definitions"
		:depends-on ("lolh-worklog-package"))
	       (:file "lolh-worklog-classes"
		:depends-on ("lolh-worklog-definitions"))
	       (:file "lolh-worklog"
		:depends-on ("lolh-worklog-definitions"))))

(defsystem "lolh-worklog/t"
  :description "Tests for the lolh-worklog system."
  :version "0.0.10"
  :author "LOLH-LINC <lincolnlaw@mac.com"
  :license "CCO 1.0 Universal"
  :depends-on ("fiveam" "lolh-worklog")
  :pathname "t/"
  :components ((:file "lolh-worklog-tests")))
