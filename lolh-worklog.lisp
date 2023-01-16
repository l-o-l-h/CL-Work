;;; lolh-worklog.lisp - Code acting on worklogs
;;; Time-stamp: <2023-01-16 15:29:53 minilolh3>

;;; Author: LOLH
;;; Created: 2023-01-09
;;; Version 0.0.7

;;; Commentary:
;; Code (procedures and variables) acting on worklog files.

;;; Code:

(in-package :lolh.worklog)

(defparameter +entry-separator+ "")
(defparameter +subj-verb-separator+ " --- ")
(defparameter +desc-separator+
  " ------------------------------------------------------------------------------")
(defparameter +simple-print-format+ "~A--~A | ~A | ~A | ~A --- ~A~&~A~2&")

(defparameter *work-d*
  (make-pathname :directory '(:absolute "usr" "local" "work")))
(defparameter *worklog-d*
  (merge-pathnames
   (make-pathname :directory '(:relative "worklog")) *work-d*))
(defparameter *worklog-f*
  (merge-pathnames
   (make-pathname :name "worklog.2007.otl") *worklog-d*))

(defparameter *worklog-entries* nil)

(defclass worklog-entry ()
  ((begin-datetime :accessor entry-begin-datetime
		   :initarg :begin-datetime)
   (end-datetime   :accessor entry-end-datetime
		   :initarg :end-datetime)
   (caseno         :accessor entry-caseno
		   :initarg :caseno)
   (subject        :accessor entry-subject
		   :initarg :subject)
   (verb           :accessor entry-verb
		   :initarg :verb)
   (type           :accessor entry-type
		   :initarg :type)
   (description    :accessor entry-description
		   :initarg :description)))

(defmethod worklog-entry-simple-print ((entry worklog-entry) &key (to t))
  (format to +simple-print-format+
	  (entry-begin-datetime entry)
	  (entry-end-datetime entry)
	  (entry-caseno entry)
	  (entry-type entry)
	  (entry-subject entry)
	  (entry-verb entry)
	  (entry-description entry)))

(defclass worklog-caseno-entry (worklog-entry) ())

(defmethod worklog-entry-lt ((entry1 worklog-caseno-entry)
			     (entry2 worklog-caseno-entry))
  "Less-than method for a caseno worklog entry class."
  (let ((caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string< caseno1 caseno2) t)
      ((string> caseno1 caseno2) nil)
      ((string< begin-dt1 begin-dt2) t)
      ((string> begin-dt1 begin-dt2) nil)
      ((string< end-dt1 end-dt2) t)
      ((string> end-dt1 end-dt2) nil)
      ((string< subject1 subject2) t)
      ((string> subject1 subject2) nil)
      ((string< verb1 verb2) t)
      ((string> verb1 verb2) nil)
      ((string< type1 type2) t)
      ((string> type1 type2) nil)
      ((string< desc1 desc2) t)
      ((string> desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-gt ((entry1 worklog-caseno-entry)
			     (entry2 worklog-caseno-entry))
  "Greater-than method for a caseno worklog entry class."
  (let ((caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string> caseno1 caseno2) t)
      ((string< caseno1 caseno2) nil)
      ((string> begin-dt1 begin-dt2) t)
      ((string< begin-dt1 begin-dt2) nil)
      ((string> end-dt1 end-dt2) t)
      ((string< end-dt1 end-dt2) nil)
      ((string> subject1 subject2) t)
      ((string< subject1 subject2) nil)
      ((string> verb1 verb2) t)
      ((string< verb1 verb2) nil)
      ((string> type1 type2) t)
      ((string< type1 type2) nil)
      ((string> desc1 desc2) t)
      ((string< desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-eq ((entry1 worklog-caseno-entry)
			     (entry2 worklog-caseno-entry))
  "Equal method for a caseno worklog entry class."
  (not (or (worklog-entry-lt entry1 entry2)
	   (worklog-entry-gt entry1 entry2))))

(cl-bst-set-cmp-funcs :lt #'worklog-entry-lt
		      :gt #'worklog-entry-gt
		      :eq #'worklog-entry-eq)


;; (lolh.worklog:parse-worklog-file lolh.worklog:*worklog-f*)
;; (lolh.worklog:parse-worklog-file
;;  lolh.worklog:*worklog-f*
;;  :parse-func #'lolh.worklog:parse-worklog-caseno-entries)

(defun parse-worklog-file
    (file &key (parse-func #'parse-worklog-entries))
  "Given a file, parse the contents into a data structure using a
provided parsing function.  Each worklog entry should be stored in an
instance of the 'worklog-entry' class.  The collection of worklog
entries will be stored in the global variable '*worklog-entries*'.

Because there might be different potential parsing functions, use the
:parse-func keyword to supply the desired parser.  The default parser
is 'parse-worklog-entries', which returns a list of worklog-entry
instances.

An alternate parsing function is 'parse-worklog-caseno-entries',
which places the entries into a BST sorted first by case no."
  (with-open-file (s file :if-does-not-exist :error)
    (setf *worklog-entries* (funcall parse-func s))
    t))

(defun parse-worklog-entries (s)
  "Parse and collect all worklog entries from a file opened as a stream.
Return the collection as an unordered list."
  (loop for entry = (parse-worklog-entry s)
	while entry			;entries are done when nil
	;; (describe entry)
	collect entry
	;; remove this format for working code.
	do (format t ".")))

(defun parse-worklog-caseno-entries (s)
  "Parse and collect all worklog caseno entries from a file opened as
a stream.  Return the collection as a BST (using the lolh.utils package)
ordered by:

1. caseno
2. begin datetime
3. end datetime
4. subject
5. verb
6. type
7. description.

The BST is rooted in the lolh.utils:*cl-bst* parameter.
Equals are placed into lolh.utils:*cl-bst-eqs* parameter."
  (setf *cl-bst* (make-bst-node))
  (loop for entry = (parse-worklog-entry
		     s
		     :of-class-type 'worklog-caseno-entry)
	while entry			;entries are done when nil
	do
	   ;; (describe entry)
	   ;; (break)
	   (bst-insert!-node entry *cl-bst*)
	   ;; remove this format for working code.
	   (format t ".")
	finally
	   (return *cl-bst*)))


(defun parse-worklog-entry (s &key (of-class-type 'worklog-entry))
  "Procedure to parse a single worklog entry from a stream and return
it as an instance of a class given using keyword ':of-class-type',
for instance, 'worklog-entry (the default) or 'worklog-caseno-entry."
  (loop for line = (read-line s nil)
	while line   ; when line is nil, EOF has been reached; so stop
	;; default class type is 'worklog-entry
	;; another possibility is 'worklog-caseno-entry
	;; more to follow...
	with entry = (make-instance of-class-type)
	when (string= line +entry-separator+)
	  do
	     (return entry)
	count line into line-no
	do
	   (setf line (string-trim '(#\SPACE #\TAB) line))
	   (ecase line-no
	     (1 (setf (entry-begin-datetime entry) line))
	     (2 (setf (entry-caseno entry) line))
	     (3 (let* ((svs (search +subj-verb-separator+ line))
		       (subject (subseq line 0 svs))
		       (verb (subseq line (+ svs 5))))
		  (setf (entry-subject entry) subject)
		  (setf (entry-verb entry) verb)))
	     (4 (setf (entry-type entry) line))
	     (5 (setf (entry-description entry) (parse-description s)))
	     (6 (setf (entry-end-datetime entry) line)))))

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

(defun simple-print (entries &key (to t))
  "Print worklog entries as simply as possible to a stream :to (default is standard out)."
  (dolist (entry entries)
    (funcall #'worklog-entry-simple-print entry :to to)))

(defun simple-print-to-file (entries)
  "Print simple worklog entries to a file based upon the filename in *worklog-f*.

Look for the file ending with '.out.txt'."
  (with-open-file (s (concatenate 'string (enough-namestring *worklog-f*) ".out.txt")
		     :direction :output
		     :if-does-not-exist :create)
    (simple-print entries :to s)))

;;; End worklog-parse.lisp
