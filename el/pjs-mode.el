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
;;;; Revision 3.1  2015/12/10 14:51:39  troche
;;;; * pjs mode
;;;;  (header added automatically)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)

(defvar pjs-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)
;;    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?- "_" table)
    table)
  "Syntax table used in JavaScript mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu and keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pjs-mode-map* (make-sparse-keymap))

;; we don't want to stack definition declarations
(setq fi:maintain-definition-stack nil)

;; try to get a full symbol from the position given
(defun find-symbol-at-point (start end)
  ;; is the previous char a : ?
  (save-excursion
    (goto-char start)
    (backward-char)
    (when (looking-at ":")
      (while (and (not (or (looking-at "[ (]")
			   (looking-at "^")))
		  (> (point) (point-min)))
	(backward-char))
      (if (looking-at "^")
	  (setq start (point))
	(setq start (1+ (point))))))
  (buffer-substring-no-properties start end))
    
;; <ctrl-c .> in ojs file
;; works with ojs and lisp file and properly do the search
(defun %pjs-find-definition (tag)
  (interactive
   (if current-prefix-arg
       '(t)
     (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
  (if (string-match ":" tag)
      (fi::lisp-find-definition-common tag nil)
    (fi::lisp-find-definition-common (concat "js::" tag) nil)))
  
(defun %pjs-list-who-calls (tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
  (if (string-match ":" tag)
      (fi:list-who-calls tag)
    (fi:list-who-calls (concat "js::" tag))))

(defvar *pjs-compilation-buffer-name* "*PJS compilation traces*")

(defun trace-pjs-function(tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (fi::get-default-symbol "Lisp (un)trace function" t t)))))
  (let ((js-symbol (concat "js::" tag)))
    (fi:toggle-trace-definition js-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-derived-mode opx2-js-mode prog-mode "OPX2 javascript"
(define-derived-mode pjs-mode prog-mode "Planisware Script V2"
  :syntax-table pjs-mode-syntax-table

  ;; load a little bit of cc-mode for indentation
  (c-initialize-cc-mode t)
  (c-init-language-vars-for 'c-mode)
  (c-common-init 'c-mode)

  ;; set up syntax hightlighting
  (setup-pjs-syntax-highlighting)

  ;; custom keybindings from menu
  (define-key *pjs-mode-map* "\C-c." '%ojs-find-definition)
  (define-key *pjs-mode-map* "\C-c," 'fi:lisp-find-next-definition)
  (define-key *pjs-mode-map* "\C-cc" '%ojs-list-who-calls)
  (define-key *pjs-mode-map* "\C-ce" 'compile-ojs-file)
  (define-key *pjs-mode-map* "\C-ck" 'check-ojs-region)
  (define-key *pjs-mode-map* "\C-cr" 'compile-ojs-region)
  (define-key *pjs-mode-map* "\C-c\C-b" 'save-and-compile-ojs-file)
  (define-key *pjs-mode-map* "\C-cs" 'save-compile-and-sync-ojs-file)
  (define-key *ojs-mode-map* "\C-ct" 'trace-ojs-function)

  (define-key *pjs-mode-map* "\C-cl" 'lock-file)
  (define-key *pjs-mode-map* "\C-cu" 'unlock-file)

  ;; comment / un-comment
  (define-key *pjs-mode-map* "\C-c;" 'comment-region)
  (define-key *pjs-mode-map* "\C-c:" 'uncomment-region)

  ;; autoindentation on new line and add a closing } if needed
  (define-key *pjs-mode-map* (kbd "RET") 'newline-and-indent)
;;  (define-key *ojs-mode-map* (kbd "RET") 'ojs-mode-insert-lcurly-on-ret)
  ;; auto insert closing }
;;  (define-key *ojs-mode-map* (kbd "{") 'ojs-mode-insert-lcurly)
  
  ;; menu
  (easy-menu-define pjs-menu *pjs-mode-map* "Planisware Script Menu"
    '("Planisware Script"
      ["Compile and load file..." compile-ojs-file
       t]
      ["Check syntax of selected region" check-ojs-region
       t]	    
      ["Compile, load and synchronize file..." save-compile-and-sync-ojs-file
       t]
      ["Compile and run selected region" compile-ojs-region
       t]	    
      ["Find function definition..." %ojs-find-definition
       t]
      ["Trace/Untrace function..." trace-ojs-function
       t]
      ))

  ;; custom keymap
  (use-local-map *pjs-mode-map*)  
  
  ;; rebuild  function and vars cache on save and when we open a file
;;  (add-hook 'after-save-hook 'pjs-reset-cache nil t)
;;  (add-hook 'find-file-hook 'pjs-reset-cache nil t)
  )

;; pjs files are pjs modes
(setq auto-mode-alist (cons '("\\.pjs" . pjs-mode) auto-mode-alist))
