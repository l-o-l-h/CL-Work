;;; lolh-worklog.lisp - Code acting on worklogs
;;; Time-stamp: <2023-01-31 23:49:42 minilolh3>

;;; Author: LOLH
;;; Created: 2023-01-09
;;; Version 0.0.10

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
	     (worklog-entry-set-cmp-funcs)
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

(defun find-first (bst data)
  "Return the BST rooted in the given data.
Data should be an item held by the top slot of the class type held in the BST.
This function calls a method based upon the type of data held in the BST.
For example, assume the BST holds 'worklog-caseno-entry data items.
Data should be a caseno, such as \"050305\"."
  (worklog-entry-find-first bst (bst-node-data bst) data))

(defun traverse-first-filtered (bst data)
  "Traverse that portion of a BST that matches the data.
'find-first' locates the first entry that matches data.
Thereafter the procedure filters out any entries that don't match.
For example, caseno = 210501."
  (let ((b (find-first bst data))
	(d (make-instance (type-of (bst-node-data bst)) :caseno data)))
    (labels ((trav (b1 d1)
	       (when (null b1) (return-from trav))
	       (trav (bst-node-left b1) d1)
	       (when (worklog-entry-eq-top (bst-node-data b1) d1)
		 ;; (describe (bst-node-data b1))
		 (funcall #'worklog-entry-simple-print (bst-node-data b1))
		 (format t "~%"))
	       (trav (bst-node-right b1) d1)))
      (trav b d))))

(defun trust-account (bst data)
  "Traverse that portion of a BST that matches the data.
'find-first' locates the first entry that matches data.
Thereafter the procedure filters out any entries that don't match.
For example, caseno = 210501."
  (let ((b (find-first bst data))
	(d (make-instance (type-of (bst-node-data bst)) :caseno data)))
    (labels ((trav (b1 d1)
	       (when (null b1) (return-from trav))
	       (trav (bst-node-left b1) d1)
	       (when (and
		      (worklog-entry-eq-top (bst-node-data b1) d1)
		      (string= (entry-type (bst-node-data b1)) "TRUST"))
		 ;; (describe (bst-node-data b1))
		 (funcall #'worklog-entry-simple-print (bst-node-data b1))
		 (format t "~%"))
	       (trav (bst-node-right b1) d1)))
      (trav b d))))

;;; End lolh-worklog.lisp
