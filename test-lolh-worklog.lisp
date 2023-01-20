;;; test-lolh-worklog.lisp - Simple test to check the configuration.
;;; Time-stamp: <2023-01-20 08:43:26 minilolh3>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Version 0.0.8
;;; Created: 2023-01-09

;;; Commentary:
;; From CL, (load "test-lolh-worklog")
;; and then execute (lolh.worklog:test-lolh-worklog)
;; to check that the system is configured properly.

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

(print "After (load '(test-lolh-worklog)'

execute:
- '(lolh.worklog:test-lolh-worklog)'
- '(lolh.worklog:test-lolh-wroklog-bst)'
to check that the system is configured properly.")

;;; End test-lolh-worklog.lisp
