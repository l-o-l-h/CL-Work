;;; lolh-worklog2-tests.lisp
;;; Time-stamp: <2023-02-25 23:22:44 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-25
;;; Version: 0.0.1

;;; Commentary:

;; run! : Equivalent to (explain (run TEST-SPEC)).

;;; Code:

(defpackage :lolh.worklog2.tests
  (:use
   :cl
   :fiveam
   :lolh.worklog2))

(in-package :lolh.worklog2.tests)

(def-suite :main-worklog2-tests
  :description "Main test suite for lolh.worklog2.tests package")

(def-suite :first-worklog2-tests
  :description "The first set of tests for lolh-worklog2-tests"
  :in :main-worklog2-tests)

(in-suite :first-worklog2-tests)

(test do-nothing
      (is (eq t t)))

;;; End lolh-worklog2-tests.lisp
