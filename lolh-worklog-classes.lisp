;;; lolh-worklog-classes.lisp - LOLH Worklog Classes
;;; Time-stamp: <2023-01-23 17:52:37 minilolh3>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version 0.0.9

;;; Commentary:
;; File to hold all of the class definitions and methods used by lolh.worklog
;; - worklog-entry
;; - worklog-datetime-entry
;; - worklog-caseno--entry

;; Methods sort and print differently based upon the class.

;;; Code:

(in-package :lolh.worklog)



;; CLASS=> WORKLOG-ENTRY
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
  "Procedure to parse a basic worklog-entry instance."
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




;; CLASS=> WORKLOG-DATETIME-ENTRY
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




;; CLASS=> WORKLOG-CASENO-ENTRY
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




;; CLASS=> WORKLOG-TIME-ENTRY
(defclass worklog-time-entry (worklog-entry)
  ((begin-ts :accessor begin-ts
	     :initarg :begin-ts)
   (end-ts   :accessor end-ts
	     :initarg :end-ts)
   (elapsed-time :accessor elapsed-time))
  (:documentation "A worklog-entry class that handles elapsed time."))

(defmethod initialize-instance :after ((wl-t-entry worklog-time-entry) &key)
  "A method to calculate the ts-diff value for a new instance of a
worklog-time-entry instance upon initialization."
  (setf (begin-ts wl-t-entry) (parse-timestring (entry-begin-datetime wl-t-entry) :offset (tz-offset wl-t-entry)))
  (setf (end-ts wl-t-entry) (parse-timestring (entry-end-datetime wl-t-entry) :offset (tz-offset wl-t-entry)))
  (setf (elapsed-time wl-t-entry) (ts-diff wl-t-entry)))

(defmethod dst-p ((wl-entry worklog-entry))
  "Predicate returning t if the time entries in worklog-entry are in
daylight savings time (DST)"
  (let* ((dt (entry-begin-datetime wl-entry))
	 (ts (parse-timestring dt :end 10 :offset -28800)))
    (ninth (multiple-value-list (decode-timestamp ts)))))

(defmethod tz-offset ((wl-entry worklog-entry))
  "The correct timezone offset to use depending on whether daylight
savings time is in effect on a particular day.

    A worklog entry does not contain an included time zone, so it is
parsed by local-time as a UTC time zone, and then translated to a
US/Pacific time with an included offset of either -7 hours (25200
seconds) or -8 hours (28800 seconds), depending on whether daylight
savings time was in effect.  This time must therefore be parsed with
reference to an an offset of either -7 or -8 hours.  This procedure
returns the correct offset for the begin and end datetimes of the
worklog entry based upon the parsing function figuring out which
offset is correct.  The local-time procedure 'decode-timestamp'
returns a long list of values, the tenth value of which is the offset
that is correct based upon the value of daylight savings time."
  (let* ((dt (entry-begin-datetime wl-entry))
	 (ts (parse-timestring dt :end 10 :offset -28800)))
    (tenth (multiple-value-list (decode-timestamp ts)))))

(defmethod ts-diff ((wl-entry worklog-time-entry))
  "Given a worklog entry, calculate the elapsed time between the begin
and end datetimes.  The value is stored as a 'local-time-duration' class."
  (local-time-duration:timestamp-difference (end-ts wl-entry) (begin-ts wl-entry)))

;;; End lolh-worklog-classes.lisp
