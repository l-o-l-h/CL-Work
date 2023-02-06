;;; lolh-worklog-tests.lisp
;;; Time-stamp: <2023-02-06 01:23:44 minilolh3>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-01-24
;;; Version: 0.0.11

;;; Commentary:

;; run! : Equivalent to (explain (run TEST-SPEC)).

;;; Code:

(defpackage :tests.lolh.worklog
  (:use :cl :fiveam :lolh.worklog))

(in-package :tests.lolh.worklog)

(def-suite :root-worklog-tests
  :description "Root test suite for :tests-lolh-worklog")

(def-suite :paths-worklog-tests
  :description "Tests for pathnames in lolh-worklog"
  :in :root-worklog-tests)

(in-suite :paths-worklog-tests)

(test files-exist
  (is-true *work-d*)
  (is-true *worklog-d*)
  (is-true *worklog-f*))
(test envvars-exist
      (is-true (uiop:getenvp "WORKLOG")))

(def-suite :trust-accounting-tests
  :description "Tests for trust accounting procedures."
  :in :root-worklog-tests)

(in-suite :trust-accounting-tests)

(test trust-accounting-simple-print
  (finishes (setf *worklog-entries*
		  (parse-worklog-file
		   :bst (lolh.utils:make-bst-node)
		   :file *worklog-f*
		   :class 'worklog-time-entry)))
  (finishes (simple-print-bst *worklog-entries*))
  (finishes (worklog-trust-account *worklog-entries* "210401")))

(def-suite :all-files
  :description "Tests using all files in /usr/local/work/worklog"
  :in :root-worklog-tests)

(in-suite :all-files)

(test use-all-files
  (finishes (setf *worklog-entries*
		  (parse-worklog-files))))

;;; End lolh-worklog-tests.lisp
