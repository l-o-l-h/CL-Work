;;; lolh-worklog-package.lisp - Package definition for lolh-worklog
;;; Time-stamp: <2023-02-04 16:37:10 minilolh3>

;;; Author: LOLH
;;; Created: 2023-01-16
;;; Version 0.0.10

;;; Commentary:
;; lolh.worklog package definition.

;;; Code:

(defpackage :lolh.worklog
  (:use
   :cl
   :local-time
   :lolh.utils)
  (:export
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
   :worklog-entry-set-top-cmp-funcs))

;;; End lolh-worklog-package.lisp
