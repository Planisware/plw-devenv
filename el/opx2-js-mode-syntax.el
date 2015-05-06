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
;;;; Revision 3.3  2015/05/06 14:32:14  troche
;;;; * idem
;;;;
;;;; Revision 3.2  2015/01/06 17:03:37  troche
;;;; * update of the opx2 javascript mode with (almost) intelligent syntax highlighting and completion
;;;; * update of the javascript evaluator, now you don't exit it if you have a lisp error
;;;;
;;;; Revision 3.1  2014/12/22 17:50:23  troche
;;;;  Proper syntax highlighting for ojs
;;;;  (header added automatically)
;;;;

(defun js--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

;; constants
(defconst opx2-js-font-lock-constants
  (js--regexp-opt-symbol
   '("this"
     "super"
     "context"
     "true"
     "false"
     "undefined"
     )))

;; languages keywords
(defconst opx2-js-font-lock-keywords
  (js--regexp-opt-symbol
   '("global"
     "withUpdateCommands"
     "withnoappletrefresh"
     "sqlwithdatabasetransaction"
     "withoutalerts"
     "withoutdbrecording"
     "withoutinterrupts"
     "withoutlocking"
     "withprocesslock"
     "withobjectlock"
     "whentimeout"
     "whenthrow"
     "withmonitoring"
     "fromobject"
     "LispCall"
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
     "on new"
     "on delete"
     "on modifyafter"
     "on modifybefore"
     )))

;; ojs types
(defconst opx2-js-font-lock-types
  (js--regexp-opt-symbol
   '(
     "number"
     "integer"
     "boolean"
     "string"
     "date"
     "vector"
     "array"
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

;; use functions defined in opx2-js-cache.el

(defun search-buffer-functions (end)
  (let ((search-pattern (ojs-functions-in-buffers-regexp)))
    (re-search-forward search-pattern end t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight kernel functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ojs-kernel-functions-cache* nil)

(defvar *ojs-kernel-functions-present* t)

(defun list-ojs-kernel-functions ()
  (cond (*ojs-kernel-functions-cache*
	 *ojs-kernel-functions-cache*)
	(*ojs-kernel-functions-present*
	 (progn (setq *ojs-kernel-functions-present* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(if (fboundp 'jvs::list-all-js-functions) t nil)")))
		(when *ojs-kernel-functions-present*
		  (setq *ojs-kernel-functions-cache* (format "\\(%s\\)(" (js--regexp-opt-symbol (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(jvs::list-all-js-functions)"))))))
		*ojs-kernel-functions-cache*))
	(t
	 nil)))

(defun search-kernel-functions (end)
  (when (list-ojs-kernel-functions)
    (let ((search-pattern (list-ojs-kernel-functions)))
      (re-search-forward search-pattern end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight locally defined vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vars from the current function
(defun search-vars-from-context (end)
  (re-search-forward (js--regexp-opt-symbol (get-local-vars-for-function nil)) end t))

;; script-level and global vars.
(defun search-global-vars (end)
  (re-search-forward (ojs-vars-in-buffer-regexp) end t))

;; searches only in non string and non comments 

(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun er--point-is-in-string-p ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defvar *use-real-search* nil)

(defun re-real-search-backward (regexp limit errorp)
  (if *use-real-search*
      (let ((last-point (point))
	    ;; do a first search
	    (found-point (re-search-backward regexp limit errorp)))
	;; checks that we are moving to avoid loops
	(while (and found-point
		    (< (point) last-point)
		    (or (er--point-is-in-comment-p)
			(er--point-is-in-string-p)))
	  (setq last-point found-point)
	  (setq found-point (re-search-backward regexp limit errorp)))
	found-point))
  (re-search-backward regexp limit errorp))

(defun re-real-search-forward (regexp limit errorp)
  (if *use-real-search*
      (let ((last-point (point))
	    ;; do a first search
	    (found-point (re-search-forward regexp limit errorp)))
	;; checks that we are moving to avoid loops
	(while (and found-point
		    (> (point) last-point)
		    (or (er--point-is-in-comment-p)
			(er--point-is-in-string-p)))
	  (setq last-point found-point)
	  (setq found-point (re-search-forward regexp limit errorp)))
	found-point))
  (re-search-forward regexp limit errorp))

(defun start-of-function ()
  ;; get the start of the function
  (save-excursion
    (let ((start-point (point))
	  (function-start (re-real-search-backward *ojs-function-start-regexp* nil t)))
      ;; we have a function-start, check that we don't have a } between the point and the function start
      (when function-start
	(if (string-match "^}$" (buffer-substring-no-properties function-start start-point))
	    nil
	  function-start)))))

(defun end-of-function ()
  ;; we consider we are at the { beginning the function
  ;; we try the forward-list first
  (save-excursion
    (or
     (condition-case nil
	 (progn (forward-list) (point))
       ;; return nil when we have a scan error
       (scan-error nil))
     ;; search the single }
     (re-real-search-forward "^}$" nil t)
     ;; search the beginning of the next function
     (re-real-search-forward *ojs-function-start-regexp* nil t)
     ;; if this failed, we return the end of the buffet
     (point-max))))

(defun get-local-vars-for-function (list-of-cons)
  ;; if list-of-cons is t, we return a list of cons (variable . documentation)
  ;; if it is nil, we return a list of variable
  ;; we move to the beginning of the function
  ;; test if we are inside a function ie
  ;; if we have a function/method word backward
  ;; and that this word is before the next closing } we find
  (save-excursion
    ;; we ignore errors because the forward-list can fail if parenthesis are not balanced
    ;; in that case, we do nothing.
      (let ((start-function (start-of-function)))
	(when start-function
	  (goto-char start-function)
	  (let* ((begin-args (progn (while (not (looking-at "("))
				      (forward-char))
				    (point)))
		 (end-args (progn (while (not (looking-at ")"))
				    (forward-char))
				  (point)))
		 (begin-function (progn (while (not (looking-at "{"))
					  (forward-char))
					(point)))
		 (end-function (end-of-function))
		 (function-line (buffer-substring-no-properties start-function (1+ end-args) ))
		 vars)
	    ;; arguments
	    (dolist (arg (split-string (buffer-substring-no-properties (1+ begin-args) end-args) "[ \t,]+" t))
	      (if list-of-cons
		  (push (cons arg function-line) vars)
		(push arg vars)))
	    ;; vars inside of the function
	    (goto-char begin-function)
	    (while (re-search-forward *ojs-vars-regexp* end-function t)
	      (if list-of-cons
		  (push (cons (match-string-no-properties 1) (match-string-no-properties 0)) vars)
		(push (match-string-no-properties 1) vars)))
	    vars)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax hightlighting definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-ojs-syntax-highlighting ()
  ;; syntax highlighting for ojs
  
  (setq font-locks nil)

  (setq font-lock-verbose t)

  ;; list of font-lock-keywords in the right order

  ;; Variables in the function 
  (push (cons 'search-vars-from-context font-lock-variable-name-face) font-locks)
  ;; Global vars
  (push (cons 'search-global-vars font-lock-variable-name-face) font-locks)
  ;; Variable definitions
  (push (list *ojs-vars-regexp* 1 ojs-var-definition-face) font-locks)
  ;; Functions defined in buffers
;;  (push (cons 'search-buffer-functions font-lock-function-name-face) font-locks)
  (push (list 'search-buffer-functions 1 font-lock-function-name-face) font-locks)
  ;; Kernel functions
;;  (push (cons 'search-kernel-functions ojs-kernel-functions-face) font-locks)
  (push (list 'search-kernel-functions 1 ojs-kernel-functions-face) font-locks)
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
  (push (cons opx2-js-font-lock-keywords font-lock-keyword-face) font-locks)
  ;; constants
  (push (cons opx2-js-font-lock-constants font-lock-constant-face) font-locks)
  ;; types
  (push (cons opx2-js-font-lock-types font-lock-type-face) font-locks)
  
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
	       
