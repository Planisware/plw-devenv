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

;;;; Revision 3.12  2016/03/21 13:21:50  troche
;;;; * merge from git
;;;;
;;;; Revision 3.11  2015/05/12 14:11:46  troche
;;;; * use list-all-js-functions
;;;;
;;;; Revision 3.10  2015/05/12 12:50:36  troche
;;;; * new classes completion based on kernel
;;;;
;;;; Revision 3.9  2015/05/06 16:24:01  mgautier
;;;; - fix auto-complete in js evaluator mode
;;;;
;;;; Revision 3.8  2015/05/06 14:30:33  troche
;;;; * always complete on backtab
;;;;
;;;; Revision 3.7  2015/01/06 17:03:37  troche
;;;; * update of the opx2 javascript mode with (almost) intelligent syntax highlighting and completion
;;;; * update of the javascript evaluator, now you don't exit it if you have a lisp error
;;;;
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

;;; find functions/methods defined in the current file and other open files with the same mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-functions"
    '((candidates . ojs-functions-ac-candidates)
      (document . ojs-functions-ac-document)
      (action . ojs-functions-ac-action)
      (symbol . "f ")
      (requires . -1)))

;; use the functions defined in opx2-js-cache.el
(defun ojs-functions-ac-action ()
  (message (cdr (assoc candidate (ojs-functions-in-buffers)))))

(defun ojs-functions-ac-candidates ()
  (ojs-functions-in-buffers))

(defun ojs-functions-ac-document (docstr)
  docstr)

;;;; find vars from the context : 
;;;;  local vars of the functions 
;;;;  script level defined functions
;;;;  global vars from all functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-vars"
    '((candidates . ojs-vars-ac-candidates)
      (document . ojs-vars-ac-document)
      (action . ojs-vars-ac-action)
      (symbol . "v ")
      (requires . -1)))

(defun ojs-vars-ac-action ()
  (message (cdr (assoc candidate *ojs-buffers-vars-cache*))))

(defun ojs-vars-ac-document (docstr)
  docstr)

;; we use the same function as the syntax highlighting
(defun ojs-vars-ac-candidates ()
  (append 
   (get-local-vars-for-function t)
   (ojs-vars-in-buffer)))

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
      (symbol . "c ")
      (requires . -1)))

(defvar *ojs-classes-candidates-cache* nil)

;; on reset le cache dès qu'on sauve
;;(ac-clear-variable-after-save '*ojs-classes-candidates-cache*)

;; reset du cache toutes les minutes
;;ac-clear-variable-every-minute '*ojs-functions-candidates-cache*)

(defun ojs-classes-ac-candidates ()
  *ojs-classes-candidates-cache*)

(defun ojs-classes-ac-init ()
  (or *ojs-classes-candidates-cache*
      (setq *ojs-classes-candidates-cache* (when (pjs-configuration-ok) 
					     (fi:eval-in-lisp "(jvs::list-all-js-classes \"[Oo][Pp][Xx][^0-9].*$\")")))))  

;;; generic ojs dictionary 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "ojs-dictionary"
  '((candidates . ac-dictionary-candidates)
    (symbol . "d ")))

(defvar *ac-dictionary-candidates-cache* nil)

(defun ac-dictionary-candidates ()
  *ac-dictionary-candidates-cache*)

;;; kernel javascript functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-define-source "ojs-kernel"
  '((candidates . ojs-kernel-ac-candidates)
    (document . ojs-kernel-ac-document)
    (action . ojs-kernel-ac-action)
    (symbol . "k ")
    (requires . -1)))

(defun ojs-kernel-ac-action ()
  (let ((doc (ojs-kernel-ac-document candidate)))
    (when (not (equal doc ""))
      (message doc))))

(defvar *ojs-kernel-candidates-cache* nil)
(defvar *ojs-kernel-documents-cache* nil)

(defun ojs-kernel-ac-init ()
  ;; on récupère les symboles
  (or *ojs-kernel-candidates-cache*
      (setq *ojs-kernel-candidates-cache* (when (ojs-configuration-ok) (fi:eval-in-lisp "(jvs::list-all-js-functions)"))))
  ;; on set les documents strings
  (or *ojs-kernel-documents-cache*
      (setq *ojs-kernel-documents-cache* (when (ojs-configuration-ok) (fi:eval-in-lisp "(jvs::list-js-docs)")))))
  
(defun ojs-kernel-ac-candidates ()
  *ojs-kernel-candidates-cache*
  )

(defun ojs-kernel-ac-document (name)
  (or 
   (cdr (assq (intern (downcase name)) *ojs-kernel-documents-cache*))
   "")
  )


;;;;; configuration of the hook

;; definition of a new autocomplete mode
(defun opx2-js-setup-auto-complete-mode ()
  "Setup ac-js2 to be used with auto-complete-mode."
;;  (message "Setting autocomplete")
  ;; js functions defined in the file
  (setq ac-sources nil)
  ;;(setq ac-sources '(ac-source-ojs-functions ac-source-ojs-methods ac-source-ojs-vars ac-source-ojs-kernel))
  
  (add-to-list 'ac-sources 'ac-source-ojs-functions)
  (add-to-list 'ac-sources 'ac-source-ojs-vars)
  (add-to-list 'ac-sources 'ac-source-ojs-kernel)
  (add-to-list 'ac-sources 'ac-source-ojs-dictionary)
  (add-to-list 'ac-sources 'ac-source-ojs-members)
  (add-to-list 'ac-sources 'ac-source-ojs-classes)

  ;; initialize kernel completion
  (ojs-kernel-ac-init)

  ;; classes completion
  (ojs-classes-ac-init)

  ;; dictionary cache
  (setq *ac-dictionary-candidates-cache* (ac-file-dictionary (concat *opx2-network-folder-work-path* "devenv/el/dict/opx2-js-mode")))
  
  ;; display doc quickly
  (setq ac-quick-help-delay 0.1)
  ;; autocomplete only when I ask for it
  (setq ac-auto-start nil)
)

(add-hook 'opx2-js-mode-hook 'opx2-js-setup-auto-complete-mode)
(add-hook 'js-evaluator-mode-hook 'opx2-js-setup-auto-complete-mode)
