;;; lolh-worklog-tests.lisp
;;; Time-stamp: <2023-01-29 23:06:06 minilolh3>

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
  (is-true lolh.worklog::*work-d*))

;;; End lolh-worklog-tests.lisp
