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
;;;; Revision 3.5  2014/12/16 08:40:24  troche
;;;; * debug
;;;;
;;;; Revision 3.4  2014/12/15 18:10:00  troche
;;;; * OPX2 javascript menu in Emacs
;;;; * New functions to compile script
;;;;
;;;; Revision 3.3  2014/10/31 15:05:57  troche
;;;; * autocompletion for ojs files in emacs (requires sc8567 v3.12)
;;;; ** to use, add (defvar *use-opx2-js-mode* t) to your emacs conf file before loading the common configuration
;;;;
;;;; Revision 3.2  2014/10/28 12:57:56  troche
;;;; * New opx2 javascript emacs mode.
;;;; ** Add (defvar *use-opx2-js-mode* t) to your .emacs to use
;;;; * New opx2 javascript listener based on an emacs comint mode (still in testing).
;;;; ** Add (defvar *javascript-evaluator-mode* :comint) to your .emacs
;;;;  (header added automatically)
;;;;
;;; new emacs mode for opx2 javascript

;; font-lock-function-name-face
;; font-lock-string-face
;; font-lock-keyword-face
;; font-lock-type-face
;; font-lock-constant-face
;; font-lock-variable-name-face

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; method definition
(defconst js-method-heading-1-re
"^[ \t]*method[ \t]+\\(\\w+\\)[ \t]+\\(\\<on\\>\\)[ \t]+\\(\\w+\\)"
"Regular expression matching the start of a method header.")

;; additional keywords
(defconst opx2-js-font-lock-keywords
  '("method"
    )
)

;; additional type
(defconst opx2-js-font-lock-type
  '("hashtable"
    )
)

(require 'cc-mode)
(defvar opx2-js-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table used in JavaScript mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu and keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ojs-mode-map* (make-sparse-keymap))

;; <ctrl-c .> in ojs file
(defun ojs-find-definition(tag)
    (interactive
     (if current-prefix-arg
	 '(nil)
       (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
    (fi::lisp-find-definition-common (concat "js::" tag) nil)
    (sleep-for 0.5)
    (search-forward-regexp (concat "\\(function\\|method\\) +" tag)))

(defvar *ojs-compilation-buffer-name* "*OJS compilation traces*")

(defun compile-ojs-file ()
  (interactive)
  (do-compile-and-sync-ojs-file :compile)
  )

(defun compile-and-sync-ojs-file ()
  (interactive)
  (do-compile-and-sync-ojs-file :compile-and-sync)
  )

(defun do-compile-and-sync-ojs-file (type)
  ;; find the script name
  (let* ((script-name      (file-name-base (buffer-file-name)))
	 (script           (when (fi::lep-open-connection-p) (fi:eval-in-lisp (format "(when (fboundp 'jvs::find-script)(jvs::find-script \"%s\"))" script-name))))
	 (buffer-name *ojs-compilation-buffer-name*)
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer))
;	 (fi::listener-protocol :stream)
	 )
    (if script
	(progn
	  ;; go to the OJS compilation buffer
	  (fi::switch-to-buffer-new-screen buffer-name)
	  ;; we erase previous content
	  (erase-buffer)
	  ;; run a new listener if needed
	  (unless proc
	    (fi:open-lisp-listener
	     -1
	     *ojs-compilation-buffer-name*
	     ))
					;(function
					;	(lambda (proc)
					;	  (let ((str 
					;		 (format "%d\n%d\n"
					;			 -1 ;;(fi::session-id session)
					;			 (fi::tcp-listener-generation proc))))
					;	    (message str)
					;	    str)))))
	  (cond ((eq type :compile)
		 (process-send-string *ojs-compilation-buffer-name* (format "(:rjs \"%s\")" script)))
		((eq type :compile-and-sync)
		 (process-send-string *ojs-compilation-buffer-name* (format "(:sjs \"%s\")" script))))
	  )
      (message "Script %s not found" script-name))))

(defun trace-ojs-function(tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (fi::get-default-symbol "Lisp (un)trace function" t t)))))
  (let ((js-symbol (concat "js::" tag)))
    (fi:toggle-trace-definition js-symbol)))

(defun set-ojs-opx2-js-mode-hook()
  (define-key *ojs-mode-map* "\C-c." 'ojs-find-definition)
  (define-key *ojs-mode-map* "\C-ce" 'compile-ojs-file)
  (define-key *ojs-mode-map* "\C-cs" 'compile-and-sync-ojs-file)
  (define-key *ojs-mode-map* "\C-ct" 'trace-ojs-function))

;; menu
(easy-menu-define ojs-menu *ojs-mode-map* "OPX2 Javascript Menu"
  '("OPX2 Javascript"
    ["Compile and load file..." compile-ojs-file
     t]
    ["Compile, load and synchronize file..." compile-and-sync-ojs-file
     t]
    ["Find function definition..." ojs-find-definition
     t]
    ["Trace/Untrace function..." trace-ojs-function
     t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode opx2-js-mode js-mode "OPX2 javascript"
  :syntax-table opx2-js-mode-syntax-table

  ;; syntax highlighting
  ;; new keywords
  (dolist (kw opx2-js-font-lock-keywords)
    (font-lock-add-keywords nil (list (cons kw font-lock-keyword-face))))

  ;; new types
  (dolist (kw opx2-js-font-lock-type)
    (font-lock-add-keywords nil (list (cons kw font-lock-type-face))))

  ;; method definition 
  (font-lock-add-keywords nil (list 
			       (list js-method-heading-1-re 1 font-lock-function-name-face)
			       (list js-method-heading-1-re 2 font-lock-keyword-face)
			       (list js-method-heading-1-re 3 font-lock-type-face)
			       ))

  ;; method arguments
  (font-lock-add-keywords nil (list 
			       (list
				(concat "\\<method\\>\\([ \t]+\\w+\\)?[ \t]*\\<on\\>\\([ \t]+\\w+\\)?([ \t]*\\w")
				(list "\\(\\w+\\)\\([ \t]*).*\\)?"
				      '(backward-char)
				      '(end-of-line)
				      '(1 font-lock-variable-name-face)))))

  ;; custom keymap
  (use-local-map *ojs-mode-map*)
  
)

;; kludge : in opx2 script, the first line sets the mode to C++, and we want to avoid that
;; so we call our function from the c++ mode hook
(defun override-c++-mode ()
  (when (equal (downcase (substring buffer-file-name -3 nil)) "ojs")
    (opx2-js-mode)))

(when *use-opx2-js-mode*
  (add-hook 'c++-mode-hook 'override-c++-mode))

;; replace for new files
(defun put-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]"
  (let ((pair (assoc item alist)))
    (if pair
	(progn
	  (setcdr pair value)
	  alist)
      (cons (cons item value) alist)
      )))

(add-hook 'opx2-js-mode-hook 'set-ojs-opx2-js-mode-hook)

