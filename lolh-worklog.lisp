;;; lolh-worklog.lisp - Code acting on worklogs
;;; Time-stamp: <2023-01-21 19:02:02 wlh>

;;; Author: LOLH
;;; Created: 2023-01-09
;;; Version 0.0.9

;;; Commentary:
;; Procedures to parse a worklog file into a BST data structure, and
;; also print them out to standard-out or a file.

;;; Code:

(in-package :lolh.worklog)

(defun parse-worklog-file (&key file (class 'worklog-entry))
  "Parser for a worklog file 'file' using class 'class' for sorting.
Parsed entries are placed into a BST sorted by methods set up for
each class.  The class also determines how the BST will be printed."
  (with-open-file (s file :if-does-not-exist :error)
    (loop for entry = (parse-worklog-entry s (make-instance class))
	  initially
	     (setf *worklog-entries* (make-bst-node))
	     (format t "File: ~A~&Class: ~A~2&" file class)
	  finally
	     (format t "~2&Entries: ~A~&" entries)
	  while entry
	  count entry into entries
	  do
	     (bst-insert!-node entry *worklog-entries*)
	     (format t "."))))

(defun simple-print-bst (bst &key (to t))
  "Print worklog entries held in a BST data structure simply."
  (unless bst (setf bst (make-bst-node)))
  (unless (empty-bst-node bst)
    (simple-print-bst (bst-node-left bst) :to to)
    (funcall #'worklog-entry-simple-print (bst-node-data bst) :to to)
    (simple-print-bst (bst-node-right bst) :to to)))

(defun simple-print-bst-to-file (bst)
  "Print worklog entries held in a Bst data structure to a file simply."
  (with-open-file (s (concatenate 'string (enough-namestring *worklog-f*)
				  ".out.txt")
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :overwrite)
    (format s "File created: => ~A~2&"
	    (local-time:universal-to-timestamp (get-universal-time)))
    (simple-print-bst bst :to s)))

;;; End worklog-parse.lisp
