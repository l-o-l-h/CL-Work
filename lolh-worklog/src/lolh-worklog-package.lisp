;;; lolh-worklog-package.lisp - Package definition for lolh-worklog
;;; Time-stamp: <2023-02-06 01:25:12 minilolh3>

;;; Author: LOLH
;;; Created: 2023-01-16
;;; Version 0.0.11

;;; Commentary:
;; lolh.worklog package definition.

;;; Code:

(defpackage :lolh.worklog
  (:use
   :cl
   :local-time
   :lolh.utils)
  (:import-from
   :parse-float :parse-float)
  (:export
   :parse-worklog-files
   :parse-worklog-file
   :parse-worklog-entry
   :parse-worklog-entries
   :parse-worklog-caseno-entries
   :worklog-entry
   :worklog-caseno-entry
   :worklog-datetime-entry
   :worklog-time-entry
   :worklog-entry-lt
   :worklog-entry-gt
   :worklog-entry-eq
   :worklog-entry-lt-top
   :worklog-entry-gt-top
   :worklog-entry-eq-top
   :*work-d*
   :*worklog-d*
   :*worklog-f*
   :*worklog-entries*
   :simple-print-bst
   :simple-print-bst-to-file
   :test-lolh-worklog
   :test-lolh-worklog-bst
   :worklog-entry-set-cmp-funcs
   :worklog-entry-set-top-cmp-funcs
   :worklog-trust-account))

;;; End lolh-worklog-package.lisp
