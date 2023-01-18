;;; test-lolh-worklog.lisp - Simple test to check the configuration.
;;; Time-stamp: <2023-01-17 22:47:44 wlh>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Version 0.0.7
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

(print "From CL-USER, (load '(test-lolh-worklog)'
and then execute '(lolh.worklog:test-lolh-worklog)'
to check that the system is configured properly.")

;;; End test-lolh-worklog.lisp
