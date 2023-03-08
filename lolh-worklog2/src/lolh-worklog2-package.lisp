;;; lolh-worklog2-package.lisp - Package definition for lolh.worklog2
;;; Time-stamp: <2023-03-08 08:39:50 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-23
;;; Version 0.0.1

;;; Commentary:
;; lolh.worklog2 package definition.

;;; Code:

(defpackage :lolh.worklog2
  (:use :cl :local-time :lolh.utils)
  (:import-from :parse-float
		:parse-float)
  (:export
   :worklog2-entry
   :make-worklog2-entry
   :%entry-separator%
   :%subj-verb-separator%
   :%svs-len%
   :%desc-separator%
   :%WORK%
   :%WORKLOG%))

;;; End lolh-worklog2-package.lisp
