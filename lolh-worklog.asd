(defsystem "lolh-worklog"
  :description "Procedures operating upon worklogs."
  :version "0.0.9"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CC0 1.0 Universal"
  :depends-on ("lolh-utils" "local-time" "time-interval" "local-time-duration")
  :components ((:file "lolh-worklog-package")
	       (:file "lolh-worklog-definitions"
		:depends-on ("lolh-worklog-package"))
	       (:file "lolh-worklog-classes"
		:depends-on ("lolh-worklog-definitions"))
	       (:file "lolh-worklog"
		:depends-on ("lolh-worklog-definitions"))))
