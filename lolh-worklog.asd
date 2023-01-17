(defsystem "lolh-worklog"
  :description "Procedures operating upon worklogs."
  :version "0.0.1"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CC0 1.0 Universal"
  :depends-on ("lolh-utils")
  :components ((:file "lolh-worklog-package")
	       (:file "lolh-worklog-classes"
		:depends-on ("lolh-worklog-package"))
	       (:file "lolh-worklog"
		:depends-on ("lolh-worklog-classes"))
	       (:file "test-lolh-worklog"
		:depends-on ("lolh-worklog"))))
