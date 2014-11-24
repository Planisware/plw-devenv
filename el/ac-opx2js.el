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
;;;; Revision 3.6  2014/11/24 15:49:23  troche
;;;; In emacs and xemacs, new binding C-x C-y to open a patch by typing only the number
;;;;
;;;; Revision 3.5  2014/11/04 12:46:44  troche
;;;; * get functions methods and vars from all ojs buffers
;;;;
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

;;; find functions defined in the current file and other open files with the same mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-functions"
    '((candidates . ojs-functions-ac-candidates)
      (document . ojs-functions-ac-document)
      (action . ojs-functions-ac-action)
      (symbol . "f ")
      (requires . -1)))

(defun ojs-functions-ac-action ()
  (message (cdr (assoc candidate *ojs-functions-candidates-cache*))))

(defun ojs-functions-ac-document (docstr)
  docstr)

(defvar *ojs-functions-candidates-cache* nil)

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-functions-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

;;(defvar *ojs-functions-regexp* "^[ \t]*function[ \t]+\\(\\(\\w\\|_\\)+\\)(\\(.*\\))")
(defvar *ojs-functions-regexp* "^[ \t]*function[ \t]+\\(\\(\\w\\|_\\)+\\)(\\(.*\\))")

(defun ojs-functions-ac-candidates ()
  (or *ojs-functions-candidates-cache*
      (setq *ojs-functions-candidates-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-functions-regexp*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; find methods defined in the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-methods"
    '((candidates . ojs-methods-ac-candidates)
      (document . ojs-methods-ac-document)
      (action . ojs-methods-ac-action)
      (symbol . "m ")
      (requires . -1)))

(defun ojs-methods-ac-action ()
  (message (cdr (assoc candidate *ojs-methods-candidates-cache*))))

(defun ojs-methods-ac-document (docstr)
  docstr)
  
(defvar *ojs-methods-candidates-cache* nil)

(defvar *ojs-methods-regexp* "^[ \t]*method[ \t]+\\(\\w+\\)[ \t]+\\(\\<on\\>\\)[ \t]+\\(\\w+\\)[ \t]*(\\(.*\\))")

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-methods-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-methods-ac-candidates ()
  (or *ojs-methods-candidates-cache*
      (setq *ojs-methods-candidates-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-methods-regexp*))
      ))


;;;; find vars defined in the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-vars"
    '((candidates . ojs-vars-ac-candidates)
      (document . ojs-vars-ac-document)
      (action . ojs-vars-ac-action)
      (symbol . "v ")
      (requires . -1)))

(defun ojs-vars-ac-action ()
  (message (cdr (assoc candidate *ojs-vars-candidates-cache*))))

(defun ojs-vars-ac-document (docstr)
  docstr)

(defvar *ojs-vars-candidates-cache* nil)

;;(defvar *ojs-vars-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]+\\<=\\>[ \t]*\\(.*\\))")
;;(defvar *ojs-vars-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]*=.*")
(defvar *ojs-vars-regexp* "^.*var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\).*")

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-vars-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-vars-ac-candidates ()
  (or *ojs-vars-candidates-cache*
      (setq *ojs-vars-candidates-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-vars-regexp*))
      ))

;;; members of class
;;; we identify them by something._variablename 
;;; without a parenthesis (because it would be a method)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-members"
    '((candidates . ojs-members-ac-candidates)
      (document . ojs-members-ac-document)
      (action . ojs-members-ac-action)
      (symbol . "mb")
      (requires . -1)))

(defun ojs-members-ac-action ()
  (message (cdr (assoc candidate *ojs-members-candidates-cache*))))

(defun ojs-members-ac-document (docstr)
  docstr)

(defvar *ojs-members-candidates-cache* nil)

;;(defvar *ojs-members-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]+\\<=\\>[ \t]*\\(.*\\))")
;;(defvar *ojs-members-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]*=.*")
(defvar *ojs-members-regexp* "^[^//].*\\.\\(_\\w+\\)[ \t]*[.,;!=]+")

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-members-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-members-ac-candidates ()
  (or *ojs-members-candidates-cache*
      (setq *ojs-members-candidates-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-members-regexp*))
      ))

;;; opx2 custom classes 
;;; strings of the form opxsomething
;;; without a parenthesis (because it would be a method)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-classes"
    '((candidates . ojs-classes-ac-candidates)
;      (document . ojs-classes-ac-document)
;      (action . ojs-classes-ac-action)
      (symbol . "c ")
      (requires . -1)))

(defun ojs-classes-ac-action ()
  (message (cdr (assoc candidate *ojs-classes-candidates-cache*))))

(defun ojs-classes-ac-document (docstr)
  docstr)

(defvar *ojs-classes-candidates-cache* nil)

;;(defvar *ojs-classes-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]+\\<=\\>[ \t]*\\(.*\\))")
;;(defvar *ojs-classes-regexp* "^[ \t]*var[ \t]+\\(\\w+\\)[ \t]*=.*")
;;(defvar *ojs-classes-regexp* "^[^//].*\\.\\(_\\w+\\)[ \t]*[.,;)!=]*")
(defvar *ojs-classes-regexp* "\\([oO][pP][xX]\\w+\\)")

;; on reset le cache dès qu'on sauve
(ac-clear-variable-after-save '*ojs-classes-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-classes-ac-candidates ()
  (or *ojs-classes-candidates-cache*
      (setq *ojs-classes-candidates-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-classes-regexp*))
      ))

;;; generic ojs dictionary 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-dictionary"
  '((candidates . ac-dictionary-candidates)
    (symbol . "d ")))

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
    (symbol . "k ")
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
      ;;(goto-char 0)
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
      (when (derived-mode-p (buffer-local-value 'major-mode buffer))
	(with-current-buffer buffer
	  (let ((found (ojs-find-candidates-from-regexp regexp candidates)))
	    (setq candidates (append candidates found))))))
    candidates))

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
  (add-to-list 'ac-sources 'ac-source-ojs-members)
  (add-to-list 'ac-sources 'ac-source-ojs-classes)

  ;; initialize kernel completion
  (ojs-kernel-ac-init)

  ;; dictionary cache
  (setq *ac-dictionary-candidates-cache* (ac-file-dictionary (concat *opx2-network-folder-work-path* "devenv/eldict/opx2-js-mode")))
  
  ;; display doc quickly
  (setq ac-quick-help-delay 0.1)
  ;; autocomplete only when I ask for it
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
