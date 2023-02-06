;;; lolh-worklog-tests.lisp
;;; Time-stamp: <2023-02-05 22:04:34 minilolh3>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-01-24
;;; Version: 0.0.10

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

(def-suite :trust-accounting-tests
  :description "Tests for trust accounting procedures."
  :in :root-worklog-tests)

(in-suite :trust-accounting-tests)

(test trust-accounting-simple-print
  (finishes (setf *worklog-entries*
		  (parse-worklog-file :file *worklog-f*
				      :class 'worklog-time-entry)))
  (finishes (simple-print-bst *worklog-entries*))
  (finishes (worklog-trust-account *worklog-entries* "210401")))

;;; End lolh-worklog-tests.lisp
