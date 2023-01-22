;;; lolh-worklog-classes.lisp - LOLH Worklog Classes
;;; Time-stamp: <2023-01-21 18:40:09 wlh>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version 0.0.8

;;; Commentary:
;; File to hold all of the class definitions and methods used by lolh-worklog

;;; Code:

(in-package :lolh.worklog)

(defparameter +entry-separator+ "")
(defparameter +subj-verb-separator+ " --- ")
(defparameter +desc-separator+
  " ------------------------------------------------------------------------------")
(defparameter +simple-print-format+ "~A--~A | ~A | ~A | ~A --- ~A~&~A~2&")
(defparameter +simple-print-datetime-format+ "~A--~A (~A) | ~A | ~A | ~A --- ~A~&~A~2&")
(defparameter +simple-print-caseno-format+ "~A | ~A~&~A--~A~&~A --- ~A~2&~A~2&")

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
		   :initarg :description))
  (:documentation "The worklog-entry base clase."))

(defmethod parse-worklog-entry (s (entry worklog-entry))
  (loop for line = (read-line s nil)
	while line
	when (string= line +entry-separator+)
	  do (return entry)
	count line into line-count
	do
	   (setf line (string-trim '(#\SPACE #\TAB) line))
	   (ecase line-count
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


(defmethod worklog-entry-simple-print ((entry worklog-entry) &key (to t))
  "A basic format function to print an entry simply."
  (format to +simple-print-format+
	  (entry-begin-datetime entry)
	  (entry-end-datetime entry)
	  (entry-caseno entry)
	  (entry-type entry)
	  (entry-subject entry)
	  (entry-verb entry)
	  (entry-description entry)))

(defmethod worklog-entry-lt ((entry1 worklog-entry)
			     (entry2 worklog-entry))
  "Less-than method for a worklog entry class."
  (let ((begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string< begin-dt1 begin-dt2) t)
      ((string> begin-dt1 begin-dt2) nil)
      ((string< end-dt1 end-dt2) t)
      ((string> end-dt1 end-dt2) nil)
      ((string< caseno1 caseno2) t)
      ((string> caseno1 caseno2) nil)
      ((string< subject1 subject2) t)
      ((string> subject1 subject2) nil)
      ((string< verb1 verb2) t)
      ((string> verb1 verb2) nil)
      ((string< type1 type2) t)
      ((string> type1 type2) nil)
      ((string< desc1 desc2) t)
      ((string> desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-gt ((entry1 worklog-entry)
			     (entry2 worklog-entry))
  "Greater-than method for a worklog entry class."
  (let ((begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string> begin-dt1 begin-dt2) t)
      ((string< begin-dt1 begin-dt2) nil)
      ((string> end-dt1 end-dt2) t)
      ((string< end-dt1 end-dt2) nil)
      ((string> caseno1 caseno2) t)
      ((string< caseno1 caseno2) nil)
      ((string> subject1 subject2) t)
      ((string< subject1 subject2) nil)
      ((string> verb1 verb2) t)
      ((string< verb1 verb2) nil)
      ((string> type1 type2) t)
      ((string< type1 type2) nil)
      ((string> desc1 desc2) t)
      ((string< desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-eq ((entry1 worklog-entry)
			     (entry2 worklog-entry))
  "Equal method for a datetime worklog entry class."
  (not (or (worklog-entry-lt entry1 entry2)
	   (worklog-entry-gt entry1 entry2))))

(defclass worklog-datetime-entry (worklog-entry)
  ((elapsed :accessor elapsed
	    :initarg :elapsed))
  (:documentation "An entry specialized to work with datetimes as the 
initial sort."))

(defmethod worklog-entry-simple-print
    ((entry worklog-datetime-entry) &key (to t))
  "A basic format function to print a datetime entry simply."
  (format to +simple-print-datetime-format+
	  (entry-begin-datetime entry)
	  (entry-end-datetime entry)
	  (elapsed entry)
	  (entry-caseno entry)
	  (entry-type entry)
	  (entry-subject entry)
	  (entry-verb entry)
	  (entry-description entry))  )

(defmethod worklog-entry-lt ((entry1 worklog-datetime-entry)
			     (entry2 worklog-datetime-entry))
  "Less-than method for a datetime worklog entry class."
  (let ((begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string< begin-dt1 begin-dt2) t)
      ((string> begin-dt1 begin-dt2) nil)
      ((string< end-dt1 end-dt2) t)
      ((string> end-dt1 end-dt2) nil)
      ((string< caseno1 caseno2) t)
      ((string> caseno1 caseno2) nil)
      ((string< subject1 subject2) t)
      ((string> subject1 subject2) nil)
      ((string< verb1 verb2) t)
      ((string> verb1 verb2) nil)
      ((string< type1 type2) t)
      ((string> type1 type2) nil)
      ((string< desc1 desc2) t)
      ((string> desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-gt ((entry1 worklog-datetime-entry)
			     (entry2 worklog-datetime-entry))
  "Greater-than method for a datetime worklog entry class."
  (let ((begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string> begin-dt1 begin-dt2) t)
      ((string< begin-dt1 begin-dt2) nil)
      ((string> end-dt1 end-dt2) t)
      ((string< end-dt1 end-dt2) nil)
      ((string> caseno1 caseno2) t)
      ((string< caseno1 caseno2) nil)
      ((string> subject1 subject2) t)
      ((string< subject1 subject2) nil)
      ((string> verb1 verb2) t)
      ((string< verb1 verb2) nil)
      ((string> type1 type2) t)
      ((string< type1 type2) nil)
      ((string> desc1 desc2) t)
      ((string< desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-eq ((entry1 worklog-datetime-entry)
			     (entry2 worklog-datetime-entry))
  "Equal method for a datetime worklog entry class."
  (not (or (worklog-entry-lt entry1 entry2)
	   (worklog-entry-gt entry1 entry2))))

(defclass worklog-caseno-entry (worklog-entry) ()
  (:documentation "An entry specialized to work with caseno's."))

(defmethod worklog-entry-lt ((entry1 worklog-caseno-entry)
			     (entry2 worklog-caseno-entry))
  "Less-than method for a caseno worklog entry class."
  (let ((caseno1 (entry-caseno entry1))
	(caseno2 (entry-caseno entry2))
	(type1 (entry-type entry1))
	(type2 (entry-type entry2))
	(begin-dt1 (entry-begin-datetime entry1))
	(begin-dt2 (entry-begin-datetime entry2))
	(end-dt1 (entry-end-datetime entry1))
	(end-dt2 (entry-end-datetime entry2))
	(subject1 (entry-subject entry1))
	(subject2 (entry-subject entry2))
	(verb1 (entry-verb entry1))
	(verb2 (entry-verb entry2))
	(desc1 (entry-description entry1))
	(desc2 (entry-description entry2)))
    (cond
      ((string< caseno1 caseno2) t)
      ((string> caseno1 caseno2) nil)
      ((string< type1 type2) t)
      ((string> type1 type2) nil)
      ((string< begin-dt1 begin-dt2) t)
      ((string> begin-dt1 begin-dt2) nil)
      ((string< end-dt1 end-dt2) t)
      ((string> end-dt1 end-dt2) nil)
      ((string< subject1 subject2) t)
      ((string> subject1 subject2) nil)
      ((string< verb1 verb2) t)
      ((string> verb1 verb2) nil)
      ((string< desc1 desc2) t)
      ((string> desc1 desc2) nil)
      (t nil))))

(defmethod worklog-entry-simple-print ((entry worklog-caseno-entry) &key (to t))
  "A basic format function to print an entry simply."
  (format to +simple-print-caseno-format+
	  (entry-caseno entry)
	  (entry-type entry)
	  (entry-begin-datetime entry)
	  (entry-end-datetime entry)
	  (entry-subject entry)
	  (entry-verb entry)
	  (entry-description entry)))

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

(defclass worklog-time-entry (worklog-entry)
  ((elapsed-time :accessor elapsed-time))
  (:documentation "A worklog-entry class that handles elapsed time."))
;;; End lolh-worklog-classes.lisp
