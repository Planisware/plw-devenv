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
;;;; Revision 3.2  2015/01/06 17:03:37  troche
;;;; * update of the opx2 javascript mode with (almost) intelligent syntax highlighting and completion
;;;; * update of the javascript evaluator, now you don't exit it if you have a lisp error
;;;;
;;;; Revision 3.1  2014/12/30 12:06:42  troche
;;;;  OPX2 javascript mode cache
;;;;  (header added automatically)
;;;;

;;;;; global cache for function and methods definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-function-method-definition-regexp* 
  "^[ \t]*\\(?:function\\|method\\)[ \t]+\\(\\w+\\).*$")

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar *ojs-buffers-functions-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *ojs-buffers-functions-cache-regexp* nil)

(defun ojs-functions-in-buffers ()
  (or *ojs-buffers-functions-cache*
      (progn (generate-ojs-functions-cache)
	     *ojs-buffers-functions-cache*)))

(defun ojs-functions-in-buffers-regexp ()
  (or *ojs-buffers-functions-cache-regexp*
      (progn (generate-ojs-functions-cache)
	     *ojs-buffers-functions-cache-regexp*)))

(defun generate-ojs-functions-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-functions-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-function-method-definition-regexp*))
  ;; regexp cache
  (setq *ojs-buffers-functions-cache-regexp*
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-functions-cache*
				     collect (car item)))))

;;;;; buffer dependant cache for script-level var definitions and global vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-file-vars-regexp*  
  "^var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")

(defconst *ojs-global-vars-regexp*  
  "^[ \t]*global[ \t]+var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")

;; used for autocomplete 
(defvar *ojs-buffers-vars-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *ojs-buffers-vars-cache-regexp* nil)

(defun ojs-vars-in-buffer ()
  (interactive)
  (or *ojs-buffers-vars-cache*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache*)))

(defun ojs-vars-in-buffer-regexp ()
  (or *ojs-buffers-vars-cache-regexp*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache-regexp*)))

(defun generate-ojs-vars-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-vars-cache* (append 
				  (ojs-find-candidates-from-regexp *ojs-file-vars-regexp*)
				  (ojs-find-candidates-from-regexp-in-buffers *ojs-global-vars-regexp*)))
  ;; regexp cache
  (setq *ojs-buffers-vars-cache-regexp*
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-vars-cache*
				     collect (car item))))
  )

;;;;; global cache for kernel function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; cache reset on file save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ojs-reset-cache ()
  (setq *ojs-buffers-functions-cache* nil)
  (setq	*ojs-buffers-functions-cache-regexp* nil)
  (setq *ojs-buffers-vars-cache* nil)
  (setq *ojs-buffers-vars-cache-regexp* nil))

;;;;; generic function to find candidates based on a regexp 
;;;;; it returns a candidates list containing cons (name . documentation) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ojs-find-candidates-from-regexp (regexp &optional already-matched)
  (let (candidate
	candidates
	(point (point))
	)
    (save-excursion
      ;; Search forward from the line after the current one
      (forward-line 1)
      (while  (re-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; search backward from the start of the current line
      (goto-char point)
      (beginning-of-line)
      (while  (re-search-backward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; Search forward
      (nreverse candidates))))

(defun ojs-find-candidates-from-regexp-in-buffers (regexp )
  (let (candidates)
    (dolist (buffer (buffer-list))
      (when (or (derived-mode-p (buffer-local-value 'major-mode buffer))
		(equal (buffer-local-value 'major-mode buffer) 'opx2-js-mode))
	(with-current-buffer buffer
	  (let ((found (ojs-find-candidates-from-regexp regexp candidates)))
	    (setq candidates (append candidates found))))))
    candidates))
