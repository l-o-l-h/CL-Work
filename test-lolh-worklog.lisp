;;; test-lolh-worklog.lisp - Simple test to check the configuration.
;;; Time-stamp: <2023-01-21 07:54:23 minilolh3>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Version 0.0.8
;;; Created: 2023-01-09

;;; Commentary:
;; A few little functions to test that the system lolh-worklog
;; is working.

;;; Code:
(require :lolh-worklog)

(in-package :lolh.worklog)

(defun test-lolh-worklog ()
  (parse-worklog-file *worklog-f*)
  (simple-print *worklog-entries*))

(defun test-lolh-worklog-bst ()
  (parse-worklog-file *worklog-f*
		      :parse-func #'parse-worklog-entries-by-class
		      :use-class 'worklog-caseno-entry)
  (simple-print-bst-to-file *worklog-entries*))

(print "After (load \"test-lolh-worklog\")

execute:
- '(lolh.worklog:test-lolh-worklog)'
- '(lolh.worklog:test-lolh-worklog-bst)'
to check that the system is configured properly.")

;;; End test-lolh-worklog.lisp
