
(in-package :lolh.worklog)

(defun test-lolh-worklog ()
  (parse-worklog-file *worklog-f*)
  (simple-print *worklog-entries*))
