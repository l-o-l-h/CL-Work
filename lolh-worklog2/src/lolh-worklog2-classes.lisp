;;; lolh-worklog2-classes.lisp - LOLH Worklog2 Classes
;;; Time-stamp: <2023-02-27 20:37:49 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-24
;;; Version 0.0.1

;;; Commentary:
;; File to hold all of the class definitions and methods used by lolh.worklog2
;; - worklog-entry
;; - worklog-datetime-entry
;; - worklog-caseno--entry

;; Methods sort and print differently based upon the class.

;;; Code:

(in-package :lolh.worklog2)

(defclass worklog2-entry (rb-elem)
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
		   :initarg :description))
  (:documentation "The worklog-entry base clase."))

(defun make-worklog2-entry (&key begin-dt end-dt caseno subject verb type desc)
  "A constructor for an instance of the worklog2-entry class type."
  (make-instance 'worklog2-entry
		 :begin-datetime begin-dt
		 :end-datetime end-dt
		 :caseno caseno
		 :subject subject
		 :verb verb
		 :type type
		 :description desc))

(defmethod parse-worklog2-entry (s (entry worklog2-entry))
  "Procedure to parse and create an instance of a worklo2-entry.
S is a stream containing possibly multiple worklog entries.
An Entry contain a:
- beginning datetime (a string containing date-time string in xxx
  format)
- case no (a string of the form yytt##)
- subject --- verb (two strings separated by three dashs
- type (a string containing certain specified types))
- description (a string of indefinite length separated by two line
  separators)
- end datetime (a string)"
  (loop for line = (read-line s nil)
	while line
	when (string= %entry-separator% line)
	  do (return entry)
	count line into line-count
	do
	   (setf line (string-trim '(#\SPACE #\TAB) line))
	   (ecase line-count
	     (1 (setf (entry-begin-datetime entry) line))
	     (2 (set (entry-caseno entry) line))
	     (3 (let* ((svs (search %subj-verb-separator% line))
		       (subject (subseq line 0 svs))
		       (verb (subseq line (+ svs %svs-len%))))
		  (setf (entry-subject entry) subject)
		  (setf (entry-verb entry) verb)))
	     (4 (setf (entry-type entry) line))
	     (5 (setf (entry-description entry) (parse-description s)))
	     (6 (setf (entry-end-datetime entry) line)))))

(defun parse-description (s)
  "Parse a description section (that portion of a worklog entry
bounded by separators of 78 dashes)."
  (loop for line = (read-line s)
	with full-desc = ""
	until (string= %desc-separator% line)
	do
	   (setf full-desc (concatenate 'string full-desc line))
	finally (return (string-trim '(#\SPACE #\TAB) full-desc))))

;;; End lolh-worklog2-classes.lisp
