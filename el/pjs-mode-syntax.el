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
;; constants
(defconst pjs-font-lock-constants
  (js--regexp-opt-symbol
   '("this"
     "super"
     "context"
     "true"
     "false"
     "undefined"
     )))

;; languages keywords
(defconst pjs-font-lock-keywords
  (js--regexp-opt-symbol
   '("namespace"
     "class"
     "with"
     "where"
     "order by"
     "group by"
     "inverse"
     "break"
     "case"
     "catch"
     "else"
     "for"
     "goto"
     "if"
     "instanceof"
     "new"
     "return"
     "switch"
     "try"
     "typeof"
     "var"
     "while"
     "foreach"
     "method"
     "function"
     )))

;; ojs types
(defconst pjs-font-lock-types
  (js--regexp-opt-symbol
   '(
     "int32"
     "int"
     "longfloat"
     "shortfloat"
     "string"
     "vector"
     "boolean"
     "hashtable"
     )))

;; method definition
(defconst *ojs-method-heading*
"^[ \t]*method[ \t]+\\(\\w+\\)[ \t]+\\(\\<on\\>\\)[ \t]+\\(\\w+\\)"
"Regular expression matching the start of a method header.")

;;method arguments
(defconst *ojs-method-arguments-start*
  "\\<method\\>\\([ \t]+\\w+\\)?[ \t]*\\<on\\>\\([ \t]+\\w+\\)?([ \t]*\\w")

;; function definition
(defconst *ojs-function-heading*
"^[ \t]*function[ \t]+\\(\\w+\\)"
"Regular expression matching the start of a function header.")

;; function arguments
(defconst *ojs-function-arguments-start*
  "\\<function\\>\\([ \t]+\\w+\\)?[ \t]*([ \t]*\\w")

;; function or method regexp
(defconst *ojs-function-or-method-regexp*
  "^[ \t]*\\(\\<function\\>\\|\\<method\\>[ \t]+\\w+[ \t]+\\<on\\>[ \t]+\\w+\\)([ \t]*\\w*)")

(defconst *arguments-end*
  "\\(\\w+\\)\\([ \t]*).*\\)?")

;; vars definition
(defconst *ojs-vars-regexp* 
;;  "^.*var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")
  "^.*var[ \t]+\\(\\w+\\)")

;; new type(
(defconst *ojs-new-type-regexp*
  ".*new[ \t]+\\(\\w+\\)[ \t]*(")

;; function starts
(defconst *ojs-function-start-regexp*
  "\\(function\\|method\\|on new\\|on modifyafter\\|on modifybefore\\|on delete\\)")

;; kernel functions are in italic
(defface ojs-kernel-functions-face
    '((t 
       :inherit font-lock-function-name-face :slant italic))
    "OJS kernel fonts are displayed in italic"
    )

(defvar ojs-kernel-functions-face
  'ojs-kernel-functions-face)

;; variable defintion are in bold
(defface ojs-var-definition-face
    '((t 
       :inherit font-lock-variable-name-face :weight bold))
    "variable defintion are in bold"
    )

(defvar ojs-var-definition-face
  'ojs-var-definition-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight functions defined in the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight kernel functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defvar *ojs-kernel-functions-cache* nil)

;;(defvar *ojs-kernel-functions-present* t)

;;(defun list-ojs-kernel-functions ()
;;  (cond (*ojs-kernel-functions-cache*
;;	 *ojs-kernel-functions-cache*)
;;	(*ojs-kernel-functions-present*
;;	 (progn (setq *ojs-kernel-functions-present* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(if (fboundp 'jvs::list-all-js-functions) t nil)")))
;;		(when *ojs-kernel-functions-present*
;;		  (setq *ojs-kernel-functions-cache* (format "\\(%s\\)" (js--regexp-opt-symbol (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(jvs::list-all-js-functions)"))))))
;;		*ojs-kernel-functions-cache*))
;;	(t
;;	 nil)))

;;(defun search-kernel-functions (end)
;;  (when (list-ojs-kernel-functions)
;;    (let ((search-pattern (list-ojs-kernel-functions)))
;;      (re-search-forward search-pattern end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight locally defined vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;use the ojs one

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax hightlighting definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-pjs-syntax-highlighting ()
  ;; syntax highlighting for pjs
  
  (setq font-locks nil)

  (setq font-lock-verbose t)

  ;; list of font-lock-keywords in the right order

  ;; Functions defined in buffers
;;  (push (cons 'search-buffer-functions font-lock-function-name-face) font-locks)
  (push (list 'search-buffer-functions 1 font-lock-function-name-face) font-locks)

  ;; Kernel functions
;;  (push (cons 'search-kernel-functions ojs-kernel-functions-face) font-locks)
;;  (push (list 'search-kernel-functions 1 ojs-kernel-functions-face) font-locks)

  ;; Variables in the function 
  (push (cons 'search-vars-from-context font-lock-variable-name-face) font-locks)
  ;; Global vars
;;  (push (cons 'search-global-vars font-lock-variable-name-face) font-locks)
  ;; Variable definitions
  (push (list *ojs-vars-regexp* 1 ojs-var-definition-face) font-locks)

  ;; New type
  (push (list *ojs-new-type-regexp* 1 font-lock-type-face) font-locks)
  ;; Function definition
  (push (list *ojs-function-heading* 1 font-lock-function-name-face) font-locks)
  ;; Function arguments
  (push (list
	 (concat *ojs-function-arguments-start*)
	 (list *arguments-end*
	       '(backward-char)
	       '(end-of-line)
	       '(1 ojs-var-definition-face))) font-locks)
  ;; Method definition
  (push (list *ojs-method-heading* 1 font-lock-function-name-face) font-locks)
  (push (list *ojs-method-heading* 2 font-lock-keyword-face) font-locks)
  (push (list *ojs-method-heading* 3 font-lock-type-face) font-locks)  
  ;; Method arguments
  (push (list
	 (concat *ojs-method-arguments-start*)
	 (list *arguments-end*
	       '(backward-char)
	       '(end-of-line)
	       '(1 ojs-var-definition-face))) font-locks)
  ;; keywords
  (push (cons pjs-font-lock-keywords font-lock-keyword-face) font-locks)
  ;; constants
  (push (cons pjs-font-lock-constants font-lock-constant-face) font-locks)
  ;; types
  (push (cons pjs-font-lock-types font-lock-type-face) font-locks)
  
  ;; comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
 
  (set (make-local-variable 'font-lock-defaults)
       (list font-locks
	     nil  ;; fontify strings and comments
	     t    ;; case insensitive fontifying
	     ))

  ;; regexp to mark the beginning of a function
;;  (setq defun-prompt-regexp *ojs-function-or-method-regexp*)

  ;; fontify all the things
  (syntax-propertize (point-max))
  )

(defun force-syntax-highlighting ()
  (interactive)
  (font-lock-fontify-buffer))
