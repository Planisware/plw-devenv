;;;;
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
;;;; (when (fboundp :require-patch) (:require-patch ""))
;;;; HISTORY :
;;;; $Log$
;;;; Revision 3.2  2015/03/06 09:09:03  mgautier
;;;; - add better indent for letf and if macro. Just define *enable-tutu-indent* in your .emacs (minor-mode coming soon)
;;;;
;;;; Revision 3.1  2011/07/21 15:16:46  folli
;;;; - (plw)CVS support in emacs
;;;; - New common files shared between xemacs & emacs
;;;;  (header added automatically)
;;;;
;; -*-no-byte-compile: t; -*-

;;Force no byte compilation (elc compiled by xemacs won't be readable via emacs and
;; the other way around)

;;Fxx bindings

(defun revert-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))

(defun switch-to-listener ()
  (interactive)
  (let* ((buffer-name "*lisp-listener*")
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer)))
    (if (fi:process-running-p proc buffer-name)
	(fi::switch-to-buffer-new-screen buffer-name)
      (fi:menu-open-lisp-listener))))

(defun switch-to-common-lisp ()
  (interactive)
  (fi::switch-to-buffer-new-screen fi:common-lisp-buffer-name))

(global-set-key [f2] 'shell)
(global-set-key [f4] 'switch-to-listener)
(global-set-key [f5] 'switch-to-common-lisp)
(global-set-key [f6] 'delete-window)
(global-set-key [f11] 'revert-truncate-lines)

;; keyboard bindings

(defun indent-defun ()
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (fi:indent-sexp)))

(defvar *enable-lisp-completion* t)
(defvar *reverse-completion-shortcuts* nil)

(defun redefine-common-keys (map)
  (if *enable-lisp-completion*
      (cond (*reverse-completion-shortcuts*
	     (define-key map (kbd "<A-SPC>") 'fi:lisp-complete-symbol))
	    (t
	     (define-key map (kbd "<C-SPC>") 'fi:lisp-complete-symbol) ;;ctrl + space
	     (define-key map (kbd "<A-SPC>") 'set-mark-command))))
  ;;Make supr behave correctly in xemacs (works already this way in emacs)
  ;;(define-key map "\C-?" 'delete-char)
  )

(defun set-lisp-mode ()
  (auto-fill-mode -1) ;;Disable auto lines break

  ;; General shortcuts
  ;;(define-key fi:common-lisp-mode-map "C-M-q" 'indent-defun)
  (define-key fi:common-lisp-mode-map "\C-c\C-c" 'fi:lisp-compile-defun) ;;looks like it's not de the default

  ;;'OPX2' CVS bindings (defined here but specific to emacs or xemacs and defined in main .el file)
  (define-key fi:common-lisp-mode-map "\C-oc" 'create-opx2)
  (define-key fi:common-lisp-mode-map "\C-oe" 'edit-opx2)
  (define-key fi:common-lisp-mode-map "\C-ou" 'unedit-opx2)

  ;;Common keys for all "lisp" modes
  (redefine-common-keys fi:common-lisp-mode-map))

(add-hook 'fi:common-lisp-mode-hook 'set-lisp-mode)
(add-hook 'fi:inferior-common-lisp-mode-hook (function (lambda() (redefine-common-keys fi:inferior-common-lisp-mode-map))))
(add-hook 'fi:lisp-listener-mode-hook (function (lambda() (redefine-common-keys fi:lisp-listener-mode-map))))

;;misc eli stuff

(setq fi:eli-compatibility-mode nil) ;;do not try to connect on 9666 (acl <= 6.2)


(defvar *enable-tutu-indent* nil)
(when *enable-tutu-indent*
  (put 'letf 'fi:common-lisp-indent-hook '(like flet))
  (put 'letf 'fi:lisp-indent-hook '(like flet))
  
  (put 'if 'fi:common-lisp-indent-hook nil)
  (put 'if 'fi:lisp-indent-hook nil))
