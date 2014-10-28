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
;;; new emacs mode for opx2 javascript

;; font-lock-function-name-face
;; font-lock-string-face
;; font-lock-keyword-face
;; font-lock-type-face
;; font-lock-constant-face
;; font-lock-variable-name-face

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

(define-derived-mode opx2-js-mode javascript-mode "OPX2 javascript"
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
  
)

;; kludge : in opx2 script, the first line sets the mode to C++, and we want to avoid that
;; so we call our function from the c++ mode hook
(defun override-c++-mode ()
  (when (equal (downcase (substring buffer-file-name -3 nil)) "ojs")
    (opx2-js-mode)))

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

;; use opx2-js-mode
(defvar *use-opx2-js-mode* nil)

(when *use-opx2-js-mode*
  (add-hook 'c++-mode-hook 'override-c++-mode))

;; <ctrl-c .> in ojs file
(defun ojs-find-definition(tag)
    (interactive
     (if current-prefix-arg
	 '(nil)
       (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
    (fi::lisp-find-definition-common (concat "js::" tag) nil)
    (sleep-for 0.5)
    (search-forward-regexp (concat "\\(function\\|method\\) +" tag)))

(defun execute-code-in-lisp-listener (code) 
  (let ((current-buffer (current-buffer)))
    (execute-kbd-macro (read-kbd-macro "<f4>"))
    (insert "T")
    (execute-kbd-macro (read-kbd-macro "RET"))
    (insert code)
    (execute-kbd-macro (read-kbd-macro "RET"))
    (switch-to-buffer (buffer-name current-buffer))))

(defun eval-ojs-file()
  (interactive)
  (let ((eval-string (concat "(:compile-script-file \"" (substitute-char-in-string ?\x5c ?\x2f (buffer-file-name (current-buffer))) "\")")))
    (when (fi:eval-in-lisp "intranet::*intranet-mode*")
      (execute-code-in-lisp-listener eval-string))))

(defun trace-ojs-function(tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (fi::get-default-symbol "Lisp (un)trace function" t t)))))
  (let ((js-symbol (concat "js::" tag)))
    (fi:toggle-trace-definition js-symbol)))

(defun set-ojs-opx2-js-mode-hook()
  (define-key opx2-js-mode-map "\C-c." 'ojs-find-definition)
  (define-key opx2-js-mode-map "\C-ce" 'eval-ojs-file)
  (define-key opx2-js-mode-map "\C-ct" 'trace-ojs-function))

(add-hook 'opx2-js-mode-hook 'set-ojs-opx2-js-mode-hook)

