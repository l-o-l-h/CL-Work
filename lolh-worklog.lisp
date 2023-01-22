;;; lolh-worklog.lisp - Code acting on worklogs
;;; Time-stamp: <2023-01-21 18:18:36 wlh>

;;; Author: LOLH
;;; Created: 2023-01-09
;;; Version 0.0.8

;;; Commentary:
;; Code (procedures and variables) acting on worklog files.

;;; Code:

(in-package :lolh.worklog)

;; (lolh.worklog:parse-worklog-file lolh.worklog:*worklog-f*)
;; (lolh.worklog:parse-worklog-file
;;  lolh.worklog:*worklog-f*
;;  :parse-func #'lolh.worklog:parse-worklog-caseno-entries)

(defun parse-worklog-file (&key file (class 'worklog-entry))
  "Parser for a file 'file' using class 'class' for sorting.
Parsed entries are placed into a BST sorted by methods set up for
each class."
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
  (with-open-file (s (concatenate 'string (enough-namestring *worklog-f*)
				  ".out.txt")
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :overwrite)
    (format s "File created: => ~A~2&"
	    (local-time:universal-to-timestamp (get-universal-time)))
    (simple-print-bst bst :to s)))

;;; End worklog-parse.lisp
