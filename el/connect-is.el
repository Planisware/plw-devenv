;;;; -*- coding: windows-1252 -*-
;;;; COPYRIGHT (C) PLANISWARE $Date$ 
;;;;
;;;; All Rights Reserved
;;;;
;;;; This program and the information contained herein are confidential to
;;;; and the property of PLANISWARE and are made available only to PLANISWARE
;;;; employees for the sole purpose of conducting PLANISWARE business.
;;;;
;;;; This program and copy therof and the information contained herein shall
;;;; be maintained in strictest confidence ; shall not be copied in whole or
;;;; in part except as authorized by the employee's manager ; and shall not
;;;; be disclosed or distributed (a) to persons who are not PLANISWARE employees,
;;;; or (b) to PLANISWARE employees for whom such information is not necessary in
;;;; connection with their assigned responsabilities.
;;;;
;;;; There shall be no exceptions to the terms and conditions set forth
;;;; herein except as authorized in writing by the responsible PLANISWARE General
;;;; Manager.

;;;;
;;;; FILE    : $RCSfile$
;;;;
;;;; AUTHOR  : $Author$
;;;;
;;;; VERSION : $Id$
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile$" :id "$Id$" :version "$Revision$" :date "$Date$ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (:require-patch "")
;;;; HISTORY :
;;;; $Log$
;;;; Revision 3.1  2014/04/01 14:40:59  folli
;;;; Connect to a running IS
;;;;  (header added automatically)
;;;;
;; -*-no-byte-compile: t; -*-

(defun connect-is(host port)
  (interactive "sHost: \nnPort: ")
  (telnet host port)
  (let* ((buf (current-buffer))
	 (state :start)
	 (proc (get-buffer-process buf)))
    (when proc
      (telnet-simple-send proc "TELNET"))
    ;;search for CL-USER(2) before sending data
    (while (not (eq state :done))
      (cond ((and (search-backward "CL-USER(2):" (line-beginning-position) t)
		  (eq state :start))
	     (setq state :end)
	     (telnet-simple-send proc "(ignore-errors (let ((file (system:make-temp-file-name))) (when (excl:new-start-emacs-lisp-interface :port nil :announce-to-file file) (loop (when (probe-file file) (return))) (let ((in (open file :if-does-not-exist nil))) (when in (loop for line = (read-line in nil) while line do (format t  \"~a~%\" line)) (close in))) (delete-file file))))"))
	    ((search-backward "CL-USER(3):" (line-beginning-position) t)
	     (telnet-simple-send proc "(:exit-telnet)")
	     (setq state :done))
	    (t
	     (accept-process-output proc 0.5))))
    ;;Answer we look for is after CL-USER(2)
    (when (search-backward "CL-USER(2):" nil t)      
      (forward-line 1)
      (let ((data (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	    (file (make-temp-file "isconnect")))
	(with-temp-buffer 
	  ;;write data to a temp file and start emacs lisp interface
	  (insert data)
	  (write-region (point-min) (point-max) file)
	  (fi:start-interface-via-file host "*common-lisp*" file))
	(delete-file file)	
	(kill-buffer buf)))))
