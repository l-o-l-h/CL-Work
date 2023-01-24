;;; test-lolh-worklog.lisp - Simple test to check the configuration.
;;; Time-stamp: <2023-01-21 19:03:23 wlh>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Version 0.0.9
;;; Created: 2023-01-09

;;; Commentary:
;; A few little functions to test that the system lolh-worklog
;; is working.

;;; Code:
(require :lolh-worklog)

(in-package :lolh.worklog)

(defun test-lolh-worklog ()
  (parse-worklog-file :file *worklog-f*
		      :class 'worklog-entry))

(defun test-lolh-worklog-bst ()
  (parse-worklog-file :file *worklog-f*
		      :class 'worklog-caseno-entry)
  (simple-print-bst-to-file *worklog-entries*))

(print "After (load \"test-lolh-worklog\")

execute:
- '(lolh.worklog:test-lolh-worklog)'
- '(lolh.worklog:test-lolh-worklog-bst)'
to check that the system is configured properly.")

;;; End test-lolh-worklog.lisp
