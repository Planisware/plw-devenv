;;;; -*- coding: windows-1252 -*-
;;;; COPYRIGHT (C) PLANISWARE $Date: 2015/12/14 10:42:12 $ 
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
;;;; FILE    : $RCSfile: pjs-mode.el,v $
;;;;
;;;; AUTHOR  : $Author: troche $
;;;;
;;;; VERSION : $Id: pjs-mode.el,v 3.2 2015/12/14 10:42:12 troche Exp $
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile: pjs-mode.el,v $" :id "$Id: pjs-mode.el,v 3.2 2015/12/14 10:42:12 troche Exp $" :version "$Revision: 3.2 $" :date "$Date: 2015/12/14 10:42:12 $ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (:require-patch "")
;;;; HISTORY :
;;;; $Log: pjs-mode.el,v $
;;;; Revision 3.2  2015/12/14 10:42:12  troche
;;;; * colorization
;;;;
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

;; adapted from fi::get-default-symbol
(defun pjs-get-default-symbol (prompt up-p no-method)
  (let* ((symbol-at-point (fi::get-symbol-at-point up-p no-method))
	 (function-at-point (if (string-match ":" symbol-at-point)
				symbol-at-point
			     (find-function-at-point))))
    (if fi::use-symbol-at-point
	(list function-at-point)
      (let ((read-symbol
	     (let ((fi::original-package (fi::package)))
	       (fi::ensure-minibuffer-visible)
	       (fi::completing-read
		(if function-at-point
		    (format "%s: (default %s) " prompt function-at-point)
		  (format "%s: " prompt))
		'fi::minibuffer-complete))))
	(list (if (string= read-symbol "")
		  function-at-point
		read-symbol))))))

;; returns a cons (namespace function) 
(defun find-function-at-point ()
  (save-excursion
    (let ((word (thing-at-point 'word t)))
      (cond ((or (string-prefix-p "plw" word t)
		 (string-match (list-pjs-namespaces-regexp) word)) ;; we are looking at the namespace
	     (format "%s.%s" word (progn (forward-word (if (looking-at "\\.") 1 2))
					 (thing-at-point 'word t))))
	    (t
	     (unless (looking-back "\\.") (backward-word))
	     (format "%s.%s"
		     (or (and (looking-back "plw\\.")
			      "plw")
			 (and (looking-back (format "%s\\." (list-pjs-namespaces-regexp)))
			      (backward-word)
			      (thing-at-point 'word t))
			 (and (looking-back "\\.") ;; method call, plw namespace
			      "plw")
			 (pjs-current-namespace))
		     word))))))

;; <ctrl-c .> in ojs file
;; works with ojs and lisp file and properly do the search
(defun %pjs-find-definition (tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (pjs-get-default-symbol "Lisp locate source" t t)))))
  (interactive)
  (if (string-match ":" tag)
      (fi::lisp-find-definition-common tag nil)
    (fi::lisp-find-definition-common (if (string-prefix-p "plw." tag t)
					 (concat "js::" (substring tag (1+ (position ?. tag))))
				       (concat "js::" tag)) nil)))
  
(defun %pjs-list-who-calls (tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (pjs-get-default-symbol "List who calls" t t)))))
  (if (string-match ":" tag)
      (fi:list-who-calls tag)
    (fi:list-who-calls (if (string-prefix-p "plw." tag t)
			   (concat "js::" (substring tag (1+ (position ?. tag))))
			 (concat "js::" tag)))))

(defvar *pjs-compilation-buffer-name* "*PJS compilation traces*")

(defun trace-pjs-function(tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (pjs-get-default-symbol "Lisp (un)trace function" t t)))))
  (if (string-match ":" tag)
      (fi:toggle-trace-definition tag)
    (fi:toggle-trace-definition (if (string-prefix-p "plw." tag t)
			   (concat "js::" (substring tag (1+ (position ?. tag))))
			 (concat "js::" tag)))))
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
  (define-key *pjs-mode-map* "\C-c." '%pjs-find-definition)
  (define-key *pjs-mode-map* "\C-c," 'fi:lisp-find-next-definition)
  (define-key *pjs-mode-map* "\C-cc" '%pjs-list-who-calls)
  (define-key *pjs-mode-map* "\C-ce" 'compile-ojs-file)
  (define-key *pjs-mode-map* "\C-ck" 'check-ojs-region)
  (define-key *pjs-mode-map* "\C-cr" 'compile-ojs-region)
  (define-key *pjs-mode-map* "\C-c\C-b" 'save-and-compile-ojs-file)
  (define-key *pjs-mode-map* "\C-cs" 'save-compile-and-sync-ojs-file)
  (define-key *pjs-mode-map* "\C-ct" 'trace-pjs-function)

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
  (add-hook 'after-save-hook 'pjs-reset-cache-on-save nil t)
  (add-hook 'find-file-hook 'pjs-reset-cache-on-save nil t)
  (add-hook 'find-file-hook 'pjs-reset-cache-on-compile nil t)

  ;; activete some semantic modes
  (global-semantic-mru-bookmark-mode 1)  
  )

;; autocomplete
(require 'ac-pjs)

;; pjs files are pjs modes
(setq auto-mode-alist (cons '("\\.pjs" . pjs-mode) auto-mode-alist))
