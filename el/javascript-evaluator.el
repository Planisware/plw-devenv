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
;;;; Revision 3.1  2014/10/20 11:04:54  troche
;;;; * Correct el file
;;;;  (header added automatically)
;;;;

;; *** javascript evaluator***
(global-set-key [f3] 'switch-to-script-evaluator)

;; mode pour la coloration syntaxique

(setq *js-keywords*
 '(("Warning : " . font-lock-warning-face)
   ("Error   : " . font-lock-variable-name-face)
   ("Return  : " . font-lock-builtin-face)
   ("JS: " . font-lock-string-face))
)

(define-derived-mode js-evaluator-mode fi:lisp-listener-mode
  (setq font-lock-defaults '(*js-keywords*))
  (setq mode-name "Javascript evaluator")
)

(defun fi:open-lisp-listener (&optional buffer-number buffer-name
					setup-function command mode)
  "Open a connection to an existing Common Lisp process, started with the
function fi:common-lisp, and create a Lisp Listener (a top-level
interaction).  The Common Lisp can be either local or remote.  The name of
the buffer is \"*lisp-listener*\" with an optional suffix of \"<N>\", for
prefix arguments > 1.  If a negative prefix argument is given, then the
first \"free\" buffer name is found and used.  When called from a program,
the buffer name is the second optional argument."
  (interactive "p")
  (if fi::started-via-file
      (fi::ensure-lep-connection)
    (if (or (null fi::common-lisp-backdoor-main-process-name)
	    (not (fi:process-running-p
		  (get-process fi::common-lisp-backdoor-main-process-name)
		  buffer-name)))
	(error "Common Lisp must be running to open a lisp listener.")))
  (if fi::started-via-file
      (fi::make-tcp-connection (or buffer-name "lisp-listener")
			       buffer-number
			       'fi:lisp-listener-mode
			       fi:common-lisp-prompt-pattern
			       fi::lisp-host
			       fi::lisp-port
			       fi::lisp-password
			       (or setup-function 'fi::setup-tcp-connection))
    (let* ((buffer (process-buffer
		    (get-process fi::common-lisp-backdoor-main-process-name)))
	   (proc (fi::make-tcp-connection (or buffer-name "lisp-listener")
					  buffer-number
					  (if mode mode
					    'fi:lisp-listener-mode)
					  fi:common-lisp-prompt-pattern
					  (fi::get-buffer-host buffer)
					  (fi::get-buffer-port buffer)
					  (fi::get-buffer-password buffer)
					  (or setup-function
					      'fi::setup-tcp-connection))))
      (when command
	(process-send-string proc command))
	proc)))

(defun switch-to-script-evaluator ()
  (interactive)
  (let* ((buffer-name "*javascript-evaluator*")
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer)))
    (if (fi:process-running-p proc buffer-name)
	(fi::switch-to-buffer-new-screen buffer-name)
      (fi:open-lisp-listener -1 buffer-name 'fi::setup-tcp-connection "(format t \"Bienvenue\")(jvs::js-repl)" 'js-evaluator-mode)
      )))
