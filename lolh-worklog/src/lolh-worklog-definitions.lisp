;;; lolh-worklog-definitions.lisp - LOLH Worklog Definitions
;;; Time-stamp: <2023-01-31 01:55:45 minilolh3>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version 0.0.10

;;; Commentary:
;; File to hold all of the non-class definitions and methods
;; used by lolh.worklog package.

;;; Code:

(in-package :lolh.worklog)

(defparameter +entry-separator+ "")

(defparameter +subj-verb-separator+ " --- ")

(defparameter +desc-separator+
  " ------------------------------------------------------------------------------")

(defparameter +simple-print-format+
  "~A--~A | ~A | ~A | ~A --- ~A~&~A~2&")

(defparameter +simple-print-datetime-format+
  "~A--~A (~A) | ~A | ~A | ~A --- ~A~&~A~2&")

(defparameter +simple-print-caseno-format+
  "~A | ~A~&~A--~A~&~A --- ~A~2&~A~&----------~2&")

(defparameter +local-time-format+
  "'(:year \"-\" (:month 2) \"-\" (:day 2) \"T\" (:hour 2) \":\" (:min 2) \":\" (:sec 2))"
  "This format parses a local-time timestamp object into the datetime format used
by worklog files.")

(defparameter *work-d*
  (make-pathname :directory '(:absolute "usr" "local" "work")))

(defparameter *worklog-d*
  (merge-pathnames
   (make-pathname :directory '(:relative "worklog")) *work-d*))

(defparameter *worklog-f*
  (merge-pathnames
   (make-pathname :name "worklog.2021.otl") *worklog-d*))

(defparameter *worklog-entries* nil)

(defun parse-description (s)
  "Given a description section, parse it into a single long string.
's' is the stream reading the current worklog entry."
  (loop for line = (read-line s)
	with full-desc = ""
	until (string= line +desc-separator+)
	do
	   (setf full-desc (concatenate 'string full-desc line))
	finally
	   (return (string-trim '(#\SPACE #\TAB) full-desc))))

;;; End lolh-worklog-definitions.lisp
