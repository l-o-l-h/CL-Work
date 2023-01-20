;;; lolh-worklog-package.lisp - Package definition for lolh-worklog
;;; Time-stamp: <2023-01-20 08:36:21 minilolh3>

;;; Author: LOLH
;;; Created: 2023-01-16
;;; Version 0.0.8

;;; Commentary:
;; lolh.worklog package definition.

;;; Code:

(defpackage :lolh.worklog
  (:use :cl
	:lolh.utils)
  (:export
   :parse-worklog-file
   :parse-worklog-entry
   :parse-worklog-entries
   :parse-worklog-caseno-entries
   :worklog-entry
   :worklog-caseno-entry
   :worklog-entry-lt
   :worklog-entry-gt
   :worklog-entry-eq
   :work-d
   :worklog-d
   :*worklog-f*
   :*worklog-entries*
   :simple-print
   :test-lolh-worklog
   :test-lolh-worklog-bst))

;;; End lolh-worklog-package.lisp
