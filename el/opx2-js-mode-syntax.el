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
  
(defconst *arguments-end*
  "\\(\\w+\\)\\([ \t]*).*\\)?")

;; vars definition
(defconst *ojs-vars-regexp* 
  "^.*var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")
;; top level var definition (with no indentation)
(defconst *ojs-global-vars-regexp* 
  "^var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")

(defface ojs-kernel-functions-face
    '((t 
       :inherit font-lock-function-name-face :slant italic))
    "OJS kernel fonts are displayed in italic"
    )

(defvar ojs-kernel-functions-face
  'ojs-kernel-functions-face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax hightlighting definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-ojs-syntax-highlighting ()
  ;; syntax highlighting for ojs
  ;; font-lock-function-name-face
  ;; font-lock-string-face
  ;; font-lock-keyword-face
  ;; font-lock-type-face
  ;; font-lock-constant-face
  ;; font-lock-variable-name-face
  
  (setq font-locks nil)

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

  ;; method definition 
  (font-lock-add-keywords nil (list 
			       (list *ojs-method-heading* 1 font-lock-function-name-face)
			       (list *ojs-method-heading* 2 font-lock-keyword-face)
			       (list *ojs-method-heading* 3 font-lock-type-face)
			       ))

  ;; method arguments
  (font-lock-add-keywords nil (list 
			       (list
				(concat *ojs-method-arguments-start*)
				(list *arguments-end*
				      '(backward-char)
				      '(end-of-line)
				      '(1 font-lock-variable-name-face)))))

  ;; function definition 
  (font-lock-add-keywords nil (list 
			       (list *ojs-function-heading* 1 font-lock-function-name-face)
			       ))

  ;; function arguments
  (font-lock-add-keywords nil (list 
			       (list
				(concat *ojs-function-arguments-start*)
				(list *arguments-end*
				      '(backward-char)
				      '(end-of-line)
				      '(1 font-lock-variable-name-face)))))


  ;; var definition
  (font-lock-add-keywords nil (list
			       (list *ojs-vars-regexp* 1 font-lock-variable-name-face)
			       (list *ojs-vars-regexp* 2 font-lock-keyword-face)))			       
				

  ;; local and toplevel vars
  (font-lock-add-keywords nil (list
			       (cons 'search-vars-from-context font-lock-variable-name-face)
			       ))

  ;; functions defined in the buffer
  (font-lock-add-keywords nil (list
			       (cons 'search-buffer-functions font-lock-function-name-face)
;;			       (cons 'search-buffer-functions ojs-kernel-functions-face)
			       ))

  ;; kernel js function 
  (font-lock-add-keywords nil (list
			       (cons 'search-kernel-functions ojs-kernel-functions-face)
			       ))

  ;; fontify all the things
  (syntax-propertize (point-max))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight functions defined in the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-functions-in-buffer ()
  (save-excursion
    (let (functions)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\(function\\|method\\)[ \t]+\\(\\w+\\)"  nil t)
	(push (match-string-no-properties 2) functions))
      functions)))

(defun search-buffer-functions (end)
  (let ((search-pattern (js--regexp-opt-symbol (list-functions-in-buffer))))
    (search-re search-pattern end t)))	       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight kernel functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ojs-kernel-functions-cache* nil)

(defun list-ojs-kernel-functions ()
  (or *ojs-kernel-functions-cache*
      (setq *ojs-kernel-functions-cache* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(when (fboundp 'jvs::list-js-functions)(jvs::list-js-functions))")))))

(defun search-kernel-functions (end)
  (when (list-ojs-kernel-functions)
    (let ((search-pattern (js--regexp-opt-symbol (list-ojs-kernel-functions))))
      (search-re search-pattern end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight locally defined vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-vars-from-context (end)
  ;;
  (let ((search-pattern (js--regexp-opt-symbol (get-vars-in-context))))
    (search-re search-pattern end t)))
;;    (re-search-forward search-pattern end t)))

(defun search-re (search-pattern end error)
  (let ((search (re-search-forward search-pattern end t)))
;;    (message "Search %s found %s" search (match-string-no-properties 0))
    search
))

(defun get-vars-in-context ()
  (interactive)
  (let ((vars (append (get-local-vars-for-function) (get-global-vars-for-buffer))))
    vars)
  )

(defun get-global-vars-for-buffer()
  (interactive)
  (save-excursion
    (let (vars)
      (goto-char (point-min))
      (while (re-search-forward *ojs-global-vars-regexp* nil t)
	(push (match-string-no-properties 1) vars))
      ;;(message "Vars %s" (reverse vars))
      vars
      )))

(defun get-local-vars-for-function ()
  (interactive)
  ;; we move to the beginning of the function
  (save-excursion
    (when (re-search-backward "\\(function\\|method\\)" nil t)
      (let* ((begin-args (progn (while (not (looking-at "("))
				  (forward-char))
				(point)))
	     (end-args (progn (while (not (looking-at ")"))
				(forward-char))
			      (point)))
	     (begin-function (progn (while (not (looking-at "{"))
				      (forward-char))
				    (point)))
	     (end-function (progn (forward-list) (point)))
	     vars)
	;; arguments
	(setq vars (split-string (buffer-substring-no-properties (1+ begin-args) end-args) "[ \t,]+" t))
	;; vars inside of the function
	(goto-char begin-function)
	(while (re-search-forward *ojs-vars-regexp* end-function t)
	  (push (match-string-no-properties 1) vars))
	;;(message "Vars %s" (reverse vars))
	vars
	))))
