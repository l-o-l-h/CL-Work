;;; lolh-worklog-definitions.lisp - LOLH Worklog Definitions
;;; Time-stamp: <2023-02-05 12:17:41 minilolh3>

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

(defparameter +trust-separator+
  (make-string 100 :initial-element #\-))

(defparameter +simple-print-format+
  "~A--~A | ~A | ~A | ~A --- ~A~&~A~2&")

(defparameter +simple-print-datetime-format+
  "~A--~A (~A) | ~A | ~A | ~A --- ~A~&~A~2&")

(defparameter +simple-print-caseno-format+
  "~A | ~A~&~A--~A~&~A --- ~A~2&~A~&----------~2&")

(defparameter +simple-print-worklog-time-format+
  "~A | ~A | ~A --> ~A |~%~A --- ~A~%~A --- ~A (~A)~%~A~2%"
  "caseno | type | subject --> verb
   dt begin-end --- ts begin-end (dur)
   description")

(defparameter +simple-print-timestamp-format+
  "~&~A | ~30A | "
  "date | desc ")

(defparameter +currency-format+
  "$~:[(~; ~]~8,'*:D~,0$~:[)~; ~]"
  "Prints a number in currency format.
The comma groups by threes.
Negative numbers are identified by being enclosed in parentheses.
Asterisks pad the length to 8.")

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

(defun parse-description-with-colons (desc)
  "Parses a description line with items separated by colons, such as
$14.93 :: LOLH TRUST :: HEADNOTE CHARGE EXPENSE REIMB :: USB6831 TRUST ACCT
and returns four multiple values."
  (let ((values ())
	(start 0))
    (dotimes (x 3 (values-list (cons (subseq desc start) values)))
      (let* ((pos (position #\: desc :start start))
	     (x (string-left-trim "$"(subseq desc start (1- pos)))))
	(setf values (cons x values))
	(setf start (+ 3 pos))))))

(defun convert-to-currency (amt &key (sign t))
  "Given a number (which can be either a string or a real number,
convert the number into a specified currency form, with negative
numbers surrounded by parens."
  (when (stringp amt) (setf amt (parse-float amt)))
  (multiple-value-bind (a d)
      (truncate amt)
    (format nil +currency-format+ sign (abs a) (abs d) sign)))

(defun find-sign (verb)
  (cond
    ((search "DEPOSIT" verb) t)
    ((search "WITHDRAWAL" verb) nil)
    (t (error "The verb ~S does not contain either ~S or ~S." verb "DEPOSIT" "WITHDRAWAL"))))

;;; End lolh-worklog-definitions.lisp
