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
;;;; Revision 3.4  2015/12/18 15:09:40  troche
;;;; * better syntax colorations
;;;;
;;;; Revision 3.3  2015/12/14 15:34:18  troche
;;;; * var with types
;;;;
;;;; Revision 3.2  2015/12/14 10:42:12  troche
;;;; * colorization
;;;;
;;;; Revision 3.1  2015/12/10 14:51:39  troche
;;;; * pjs mode
;;;;  (header added automatically)
;;;;

;;; FACES ;;;

;; kernel functions : yellow in italic
(defface pjs-kernel-functions-face
    '((t 
       :inherit font-lock-function-name-face :slant italic))
    "PJS kernel fonts are displayed in italic"
    )

(defvar pjs-kernel-functions-face
  'pjs-kernel-functions-face)

;; global variable definution : Red in bold
(defface pjs-var-definition-face
    '((t 
       :inherit font-lock-variable-name-face :weight bold))
    "variable defintion are in bold"
    )

(defvar pjs-var-definition-face
  'pjs-var-definition-face)


;; functions of the current namespace

;; symbols : light blue


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
   '("order by"
     "default"
     "on"
     "let"
     "case"
     "continue"
     "protected"
     "switch"
     "false"
     "return"
     "void"
     "undefined"
     "for"
     "catch"
     "try"
     "in"
     "method"
     "extends"
     "break"
     "typeof"
     "with"
     "namespace"
     "finally"
     "instanceof"
     "true"
     "if"
     "this"
     "context"
     "group by"
     "applet"
     "else"
     "throw"
     "var"
     "do"
     "where"
     "import"
     "while"
     "function"
     "super"
     "new"
     "return values")))

;; pjs types
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
(defconst *pjs-method-heading*
"^\\s-*method\\s-+\\(\\w+\\)\\s-+\\(\\<on\\>\\)\\s-+\\(\\w+\\)"
"Regular expression matching the start of a method header.")

;;method arguments
(defconst *pjs-method-arguments-start*
  "\\<method\\>\\(\\s-+\\w+\\)?\\s-*\\<on\\>\\(\\s-+\\w+\\)?(\\s-*\\w")

;; function definition
(defconst *pjs-function-heading*
"^\\s-*function\\s-+\\(\\w+\\)"
"Regular expression matching the start of a function header.")

;; function arguments
(defconst *pjs-function-arguments-start*
  "\\<function\\>\\(\\s-+\\w+\\)?\\s-*(\\s-*\\w")

;; function or method regexp
(defconst *pjs-function-or-method-regexp*
  "^\\s-*\\(\\<function\\>\\|\\<method\\>\\s-+\\w+\\s-+\\<on\\>\\s-+\\w+\\)(\\s-*\\w*)")

(defconst *arguments-end*
  "\\(\\w+\\)\\(\\s-*).*\\)?")

;; vars definition with optional type
(defconst *pjs-vars-with-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  "^.*var\\s-*\\([[:word:].]+\\)\\s-+\\(\\w+\\)\\s-*[=;]")

(defconst *pjs-vars-no-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  "^.*var\\s-+\\(\\w+\\)\\s-*[=;]")

(defconst *pjs-var-in-with-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  "^.*var\\s-*\\([[:word:].]+\\)\\s-+\\(\\w+\\)\\s-+\\(in\\)")

(defconst *pjs-var-in-no-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  "^.*var\\s-+\\(\\w+\\)\\s-+\\(in\\)")

;; class types, plc.somethinf
(defconst *pjs-class-type*
  "plc\\.\\w+")

;; new type(
(defconst *pjs-new-type-regexp*
  ".*new\\s-+\\(\\w+\\)\\s-*(")

;; function starts
(defconst *pjs-function-start-regexp*
;;  "\\(function\\|method\\|on new\\|on modifyafter\\|on modifybefore\\|on delete\\)")
  "\\<\\(function\\|method\\)\\>")

;; class definition
(defconst *pjs-class-definition*
  ".*class\\s-+\\(\\w+\\)")

;; symbols between ##
(defconst *pjs-symbols*
  "#[[:alnum:]-_]+#")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight namespace functions (current and other namespaces) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pjs-namespace-functions-cache* nil)

(defvar *pjs-namespace-functions-present* t)

(defun list-pjs-namespace-functions ()
  (cond (*pjs-namespace-functions-cache*
	 *pjs-namespace-functions-cache*)
	(*pjs-namespace-functions-present*
	 (setq *pjs-namespace-functions-present* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(if (fboundp 'jvs::get-all-namespace-members) t nil)")))
	 (when *pjs-namespace-functions-present*
	   (setq *pjs-namespace-functions-cache* (make-hash-table :test 'equal))
	   (dolist (ns (fi:eval-in-lisp "(jvs::get-all-namespace-members)"))
	     (puthash (car ns) (js--regexp-opt-symbol (second (cdr ns))) *pjs-namespace-functions-cache*))		  
	   *pjs-namespace-functions-cache*))
	(t
	 nil)))

(defun search-pjs-current-namespace-functions (end)
  (let* ((namespace (pjs-current-namespace))
	 (functions (list-pjs-namespace-functions))
	 (ns-functions (when functions (gethash (upcase namespace) functions))))
    (re-real-search-forward (format "\\(?:%s\\.\\)?%s" namespace ns-functions) end t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight kernel functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pjs-kernel-functions-cache* nil)

(defvar *pjs-kernel-functions-present* t)

(defun list-pjs-kernel-functions ()
  (cond (*pjs-kernel-functions-cache*
	 *pjs-kernel-functions-cache*)
	(*pjs-kernel-functions-present*
	 (progn (setq *pjs-kernel-functions-present* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(if (fboundp 'jvs::list-all-js-functions) t nil)")))
		(when *pjs-kernel-functions-present*
		  (setq *pjs-kernel-functions-cache* (format "\\(\\(?:plw\\)?\\.%s\\)" (js--regexp-opt-symbol (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(jvs::list-all-js-functions)"))))))
		*pjs-kernel-functions-cache*))
	(t
	 nil)))

(defun search-pjs-kernel-functions (end)
  (when (list-pjs-kernel-functions)
    (let ((search-pattern (list-pjs-kernel-functions)))
      (re-real-search-forward search-pattern end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight functions defined in the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight class members 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pjs-current-class ()
  ;; return the current class context :
  ;; the class if we are on a method
  ;; todo: typed var
  (save-excursion
    (let ((start-function (car (function-boundaries))))
      (when start-function
	(goto-char start-function)
	(when (or (re-real-search-forward *pjs-method-heading*   (line-end-position) t)
		  (re-real-search-forward *pjs-class-definition* (line-end-position) t))
	  (let ((class-name (or (match-string-no-properties 3)
				(match-string-no-properties 1)))
		(namespace  (pjs-current-namespace)))
	    (format "%s.%s" namespace class-name)))))))
  

(defun class-members-in-function ()
  (gethash (pjs-current-class) (pjs-class-members-regexp)))

(defun search-class-members (end)
  (search-vars-in-context end 'class-members-in-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax hightlighting definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-pjs-syntax-highlighting ()
  ;; syntax highlighting for pjs
  
  (setq font-locks nil)

  (setq font-lock-verbose t)

  ;; list of font-lock-keywords in the right order

  ;; Functions defined in buffers
  
  (push (list 'search-pjs-current-namespace-functions 1 font-lock-function-name-face) font-locks)

  ;; Kernel functions
  (push (cons 'search-pjs-kernel-functions pjs-kernel-functions-face) font-locks)
  (push (list 'search-pjs-kernel-functions 1 pjs-kernel-functions-face) font-locks)

  ;; Variables in the function 
  (push (cons 'search-function-local-vars font-lock-variable-name-face) font-locks)

  ;; class members
  (push (list 'search-class-members 1 opx2-hg-getset-face) font-locks)

  ;; Global vars
  (push (cons 'search-global-vars pjs-var-definition-face) font-locks)
  
  ;; Variable definitions with type
  (push (list *pjs-vars-with-type-regexp* 1 font-lock-type-face) font-locks)
  (push (list *pjs-vars-with-type-regexp* 2 pjs-var-definition-face) font-locks)

  ;; variable definition without type
  (push (list *pjs-vars-no-type-regexp* 1 pjs-var-definition-face) font-locks)

  ;; var in with type
  (push (list *pjs-var-in-with-type-regexp* 1 font-lock-type-face) font-locks)
  (push (list *pjs-var-in-with-type-regexp* 2 pjs-var-definition-face) font-locks)
  (push (list *pjs-var-in-with-type-regexp* 3 font-lock-keyword-face) font-locks)

  ;; var in without type
  (push (list *pjs-var-in-no-type-regexp* 1 pjs-var-definition-face) font-locks)
  (push (list *pjs-var-in-no-type-regexp* 2 font-lock-keyword-face) font-locks)  

  ;; class definitions
  (push (list *pjs-class-definition* 1 font-lock-type-face) font-locks)
  
  ;; New type
  (push (list *pjs-new-type-regexp* 1 font-lock-type-face) font-locks)

  ;; symbols
  (push (list *pjs-symbols* 0 font-lock-preprocessor-face) font-locks)  
  
  ;; class types, plc.something
  (push (list *pjs-class-type* 0 font-lock-type-face) font-locks)
  
  ;; Function definition
  (push (list *pjs-function-heading* 1 font-lock-function-name-face) font-locks)
  ;; Function arguments
  (push (list
	 (concat *pjs-function-arguments-start*)
	 (list *arguments-end*
	       '(backward-char)
	       '(end-of-line)
	       '(1 pjs-var-definition-face))) font-locks)
  ;; Method definition
  (push (list *pjs-method-heading* 1 font-lock-function-name-face) font-locks)
  (push (list *pjs-method-heading* 2 font-lock-keyword-face) font-locks)
  (push (list *pjs-method-heading* 3 font-lock-type-face) font-locks)  
  ;; Method arguments
  (push (list
	 (concat *pjs-method-arguments-start*)
	 (list *arguments-end*
	       '(backward-char)
	       '(end-of-line)
	       '(1 pjs-var-definition-face))) font-locks)
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
;;  (setq defun-prompt-regexp *pjs-function-or-method-regexp*)

  ;; fontify all the things
  (syntax-propertize (point-max))
  )

(defun force-syntax-highlighting ()
  (interactive)
  (font-lock-fontify-buffer))

(defun pjs-reset-cache ()
  (ojs-reset-cache)
  (setq *pjs-buffers-class-members-cache* nil)
  (setq *pjs-buffers-class-members-cache-regexp* nil)
  (setq *pjs-namespace-functions-cache* nil)
  (setq *pjs-kernel-functions-cache*  nil)
  )
