;;; lolh-worklog2-definitions.lisp - LOLH Worklog2 Definitions
;;; Time-stamp: <2023-02-27 20:23:05 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-24
;;; Version 0.0.1

;;; Commentary:
;; File to hold all of the non-class definitions and methods
;; used by lolh.worklog2 package.

;;; Code:

(in-package :lolh.worklog2)

(defconstant %entry-separator%
  #\NEWLINE
  "The character that separates worklog entries from each other.")

(defconstant %subj-verb-separator%
  " --- "
  "The symbols the separate a subject from a verb.")

(defconstant %svs-len% (length %subj-verb-separator%))

(defconstant %desc-separator%
  (concatenate 'string
	       " "
	       (make-string 78 :initial-element #\-))
  "The line of symbols that contains a complete worklog entry
description.")

;;; End lolh-worklog2-definitions.lisp
