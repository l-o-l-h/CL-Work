(defsystem "lolh-worklog"
  :description "Procedures operating upon worklogs."
  :version "0.0.1"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CC0 1.0 Universal"
  :depends-on ("lolh-utils")
  :components ((:file "lolh-worklog")
	       (:file "test-lolh-worklog"
		      :depends-on ("lolh-worklog"))))
