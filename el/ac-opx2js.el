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
;;;; Revision 3.4  2014/10/31 16:10:14  troche
;;;; * match all items
;;;;
;;;; Revision 3.3  2014/10/31 15:09:55  troche
;;;; * debug
;;;;
;;;; Revision 3.2  2014/10/31 15:05:57  troche
;;;; * autocompletion for ojs files in emacs (requires sc8567 v3.12)
;;;; ** to use, add (defvar *use-opx2-js-mode* t) to your emacs conf file before loading the common configuration
;;;;  (header added automatically)
;;;;
(require 'auto-complete)

;;; find functions defined in the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-functions"
    '((candidates . ojs-functions-ac-candidates)
      (document . ojs-functions-ac-document)
      (action . ojs-functions-ac-action)
      (symbol . "f")
      (requires . -1)))

(defun ojs-functions-ac-action ()
  (message (ojs-functions-ac-document candidate)))

(defun ojs-functions-ac-document (symbol)
  (or 
   (get (intern symbol) :document)
   ""))

(defvar *ojs-functions-candidates-cache* nil)

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-functions-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

;;(defvar *ojs-functions-regexp* "^[ \t]*function[ \t]+\\(\\(\\w\\|_\\)+\\)(\\(.*\\))")
(defvar *ojs-functions-regexp* "^[ \t]*function[ \t]+\\(\\(\\w\\|_\\)+\\)(\\(.*\\))")

(defun ojs-functions-ac-candidates ()
  (or *ojs-functions-candidates-cache*
      (setq *ojs-functions-candidates-cache* (ojs-find-candidates-from-regexp *ojs-functions-regexp* t))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; find methods defined in the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-methods"
    '((candidates . ojs-methods-ac-candidates)
      (document . ojs-methods-ac-document)
      (action . ojs-methods-ac-action)
      (symbol . "m")
      (requires . -1)))

(defun ojs-methods-ac-action ()
  (message (ojs-methods-ac-document candidate)))

(defun ojs-methods-ac-document (symbol)
  (or 
   (get (intern symbol) :document)
   ""))

(defvar *ojs-methods-candidates-cache* nil)

(defvar *ojs-methods-regexp* "^[ \t]*method[ \t]+\\(\\w+\\)[ \t]+\\(\\<on\\>\\)[ \t]+\\(\\w+\\)[ \t]*(\\(.*\\))")

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-methods-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-methods-ac-candidates ()
  (or *ojs-methods-candidates-cache*
      (setq *ojs-methods-candidates-cache* (ojs-find-candidates-from-regexp *ojs-methods-regexp* t))
      ))


;;;; find vars defined in the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-vars"
    '((candidates . ojs-vars-ac-candidates)
      (document . ojs-vars-ac-document)
      ;(summary . opx2-js-ac-summary)
      ;(prefix . ojs-ac-prefix)
      (symbol . "v")
      (requires . -1)))

(defun ojs-vars-ac-document (symbol)
  (or 
   (get (intern symbol) :document)
   ""))

(defvar *ojs-vars-candidates-cache* nil)

;;(defvar *ojs-vars-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]+\\<=\\>[ \t]*\\(.*\\))")
(defvar *ojs-vars-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]*=.*")

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-vars-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-vars-ac-candidates ()
  (or *ojs-vars-candidates-cache*
      (setq *ojs-vars-candidates-cache* (ojs-find-candidates-from-regexp *ojs-vars-regexp* t))
      ))


;;; generic ojs dictionary 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-dictionary"
  '((candidates . ac-dictionary-candidates)
    (symbol . "d")))

(defvar *ac-dictionary-candidates-cache* nil)

(defun ac-dictionary-candidates ()
  *ac-dictionary-candidates-cache*)

;;(add-to-list 'ac-dictionary-directories (fullpath-relative-to-current-file "dict"))


;;; kernel javascript functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-kernel"
  '((candidates . ojs-kernel-ac-candidates)
    (document . ojs-kernel-ac-document)
    (action . ojs-kernel-ac-action)
    (symbol . "k")
    (requires . -1)))

(defun ojs-kernel-ac-action ()
  (message (ojs-kernel-ac-document candidate)))

(defvar *ojs-kernel-candidates-cache* nil)
(defvar *ojs-kernel-documents-cache* nil)

(defun ojs-kernel-ac-init ()
  ;; on récupère les symboles
  (or *ojs-kernel-candidates-cache*
      (setq *ojs-kernel-candidates-cache* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(when (fboundp 'jvs::list-js-functions)(jvs::list-js-functions))"))))
  ;; on set les documents strings
  (or *ojs-kernel-documents-cache*
      (setq *ojs-kernel-documents-cache* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(when (fboundp 'jvs::list-js-docs)(jvs::list-js-docs))")))))
  
(defun ojs-kernel-ac-candidates ()
  *ojs-kernel-candidates-cache*
  )

(defun ojs-kernel-ac-document (name)
  (or 
   (cdr (assq (intern (downcase name)) *ojs-kernel-documents-cache*))
   "")
  )


;;;;; generic function to find candidates based on a regexp 
;;;;; it stores the documentation in the symbol 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-doc-function"
  '((candidates . '())
    (document . '())
    ;;(summary . opx2-js-ac-summary)
    (prefix . ojs-doc-function-js-prefix)
    (requires . -1)))

(defun ojs-doc-function-js-prefix (name)
  (message "prefix : %s" name)
  nil)

;;;;; generic function to find candidates based on a regexp 
;;;;; it stores the documentation in the symbol 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ojs-find-candidates-from-regexp (regexp doc)
  (let (candidate
	candidates
	)
    (save-excursion
      ;; Search forward from the beginning of the file
      (goto-char 0)
      (while  (re-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (member candidate candidates)
	  (when (and doc candidate)
	    (put (intern candidate) :document (match-string-no-properties 0)))
	  (push candidate candidates)
	  ))
      ;; Search forward
      (nreverse candidates))))

;;;;; configuration of the hook

;; definition of a new autocomplete mode
(defun opx2-js-setup-auto-complete-mode ()
  "Setup ac-js2 to be used with auto-complete-mode."
  ;; js functions defined in the file
  (setq ac-sources nil)
  ;;(setq ac-sources '(ac-source-ojs-functions ac-source-ojs-methods ac-source-ojs-vars ac-source-ojs-kernel))
  
  (add-to-list 'ac-sources 'ac-source-ojs-functions)
  (add-to-list 'ac-sources 'ac-source-ojs-methods)
  (add-to-list 'ac-sources 'ac-source-ojs-vars)
  (add-to-list 'ac-sources 'ac-source-ojs-kernel)
  (add-to-list 'ac-sources 'ac-source-ojs-dictionary)

  ;; initialize kernel completion
  (ojs-kernel-ac-init)

  ;; dictionary cache
;;  (setq *ac-dictionary-candidates-cache* (ac-read-file-dictionary (fullpath-relative-to-current-file "devenv/el/dict/opx2-js-mode")))
  (setq *ac-dictionary-candidates-cache* (ac-file-dictionary (fullpath-relative-to-current-file "devenv/el/dict/opx2-js-mode")))
  
  ;; display doc quickly
  (setq ac-quick-help-delay 0.1)
  ;; autocomplete only when i ask for it
  (setq ac-auto-start nil)
)

(add-hook 'opx2-js-mode-hook 'opx2-js-setup-auto-complete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; autocomplete configuration

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'opx2-js-mode)
(ac-set-trigger-key "<backtab>")
(add-to-list 'ac-dictionary-directories (fullpath-relative-to-current-file "../packages/ac-dict"))
