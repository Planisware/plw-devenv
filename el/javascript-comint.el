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
;;;; Revision 3.2  2014/10/28 12:57:56  troche
;;;; * New opx2 javascript emacs mode.
;;;; ** Add (defvar *use-opx2-js-mode* t) to your .emacs to use
;;;; * New opx2 javascript listener based on an emacs comint mode (still in testing).
;;;; ** Add (defvar *javascript-evaluator-mode* :comint) to your .emacs
;;;;  (header added automatically)
;;;;

;;(when (fboundp :require-patch) (:require-patch ""))
(when (fboundp :set-source-info) (:set-source-info "$RCSfile$" :id "$Id$" :version "$Revision$" :date "$Date$"))
(when (fboundp :doc-patch) (:doc-patch "OPX2 javascript comint"))

;; function to escape strings
(defun escape-quotes (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

;; send the javascript string to jvs::evaluate-javascript-string-in-string (defined in sc8567.lisp)
(defun opx2-js-comint-input-sender (proc input)
  ;(comint-output-filter proc (format "#%s\n" input))
  (let ((js-lisp (concat "(jvs::evaluate-javascript-string-in-string \"" (escape-quotes input) "\" nil)")))
;    (comint-output-filter proc (format "%s\n" js-lisp))
    (comint-output-filter proc (format "%s\n" (fi:eval-in-lisp js-lisp)))
    (comint-output-filter proc opx2-js-comint-prompt)
    ))

;; syntax highlighting
(setq *js-keywords*
 '(("Warning : .*" . font-lock-warning-face)
   ("Error   : .*" . font-lock-variable-name-face)
   ("Return  : .*" . font-lock-type-face)
   )
)

(defconst opx2-js-comint-prompt "OJS>> ")

;; we define a new mode
(define-derived-mode
  opx2-js-comint-mode comint-mode "OPX2 javascript evaluator"
  "Run an OPX2 javascript shell."
  
  (setq comint-prompt-regexp (concat "^" (regexp-quote opx2-js-comint-prompt)))
  (setq comint-input-sender 'opx2-js-comint-input-sender)
  (setq font-lock-defaults '(*js-keywords*))
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (let ((fake-proc
	   (condition-case nil
	       (start-process "ijsm" (current-buffer) "hexl")
	     (file-error (start-process "ijsm" (current-buffer) "cat")))))
      (set-process-query-on-exit-flag fake-proc nil)
      ;; Add a silly header
      (insert "OPX2 Javascript Evaluator\n")
      (set-marker
       (process-mark fake-proc) (point))
      (comint-output-filter fake-proc opx2-js-comint-prompt))))

(defun switch-to-script-comint ()
  (interactive)
  (let* ((buffer-name "*javascript-comint*")
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 )
    (switch-to-buffer buffer-name)
    (opx2-js-comint-mode)
    ))

(when (eq *javascript-evaluator-mode* :comint)
  (global-set-key [f3] 'switch-to-script-comint))
