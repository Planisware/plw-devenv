;;;; -*- coding: windows-1252 -*-
;;;; COPYRIGHT (C) PLANISWARE $Date: 2015/12/22 12:32:06 $ 
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
;;;; FILE    : $RCSfile: pjs-mode-syntax.el,v $
;;;;
;;;; AUTHOR  : $Author: troche $
;;;;
;;;; VERSION : $Id: pjs-mode-syntax.el,v 3.5 2015/12/22 12:32:06 troche Exp $
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile: pjs-mode-syntax.el,v $" :id "$Id: pjs-mode-syntax.el,v 3.5 2015/12/22 12:32:06 troche Exp $" :version "$Revision: 3.5 $" :date "$Date: 2015/12/22 12:32:06 $ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (:require-patch "")
;;;; HISTORY :
;;;; $Log: pjs-mode-syntax.el,v $
;;;; Revision 3.5  2015/12/22 12:32:06  troche
;;;; * use local environment
;;;; * type detection (first version)
;;;;
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
     "class"
     "continue"
     "protected"
     "switch"
     "return"
     "void"
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
     "if"
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

(defconst *pjs-variable-name*
  "\\(?:\\.\\.\\.\\)?[_a-zA-Z][_a-zA-Z0-9]*")

;; method definition
(defconst *pjs-method-heading*
  (format "^\\s-*\\<method\\>\\s-+\\(%s\\)\\s-+\\(\\<on\\>\\)\\s-+\\(%s\\)" *js-function-name* *js-type*)
  "Regular expression matching the start of a method header.")

;;method arguments
(defconst *pjs-method-arguments-start*
  (format "\\<method\\>\\s-+\\(%s\\)?\\s-*\\<on\\>\\s-+\\(%s\\)?\\s-*(" *js-function-name* *js-type*))

;; function definition
(defconst *pjs-function-heading*
  (format "^\\s-*\\<function\\>\\s-+\\(%s\\)" *js-function-name*)
"Regular expression matching the start of a function header.")

;; function arguments
(defconst *pjs-function-arguments-start*
  (format "\\<function\\>\\s-+\\(%s\\)?\\s-*(" *js-function-name*))

(defconst *pjs-arguments-end*
  (format "\\s-*\\(\\<%s\\>\\)\\s-*\\(?::.+\\)?[),]" *pjs-variable-name*))

(defconst *pjs-arguments-type-end*
  (format "\\s-*\\(\\<%s\\>\\)\\s-+\\(\\<%s\\>\\)\\s-*\\(?::.+\\)?[),]" *js-type* *pjs-variable-name*))

;; vars definition with optional type
(defconst *pjs-vars-with-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-*\\(%s\\)\\s-+\\(%s\\)\\s-*[=;]" *js-type* *pjs-variable-name*))

(defconst *pjs-vars-no-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-+\\(%s\\)\\s-*[=;]" *pjs-variable-name*))

(defconst *pjs-var-in-with-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-*\\(%s\\)\\s-+\\(%s\\)\\s-+\\(in\\)" *js-type* *pjs-variable-name*))

(defconst *pjs-var-in-no-type-regexp* 
;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-+\\(%s\\)\\s-+\\(in\\)" *pjs-variable-name*))

;; class types, plc.somethinf
(defconst *pjs-class-type*
  (format "\\<plc\\>\\.%s" *js-class-name*))

;; new type(
(defconst *pjs-new-type-regexp*
  (format ".*\\<new\\>\\s-+\\(%s\\)\\s-*(" *js-type*))

;; function starts
(defconst *pjs-function-start-regexp*
;;  "\\(function\\|method\\|on new\\|on modifyafter\\|on modifybefore\\|on delete\\)")
  "\\<\\(function\\|method\\)\\>")

;; class definition
(defconst *pjs-class-definition*
  (format ".*\\<class\\>\\s-+\\(%s\\)" *js-class-name*))

;; symbols between ##
(defconst *pjs-symbols*
  "#[[:alnum:]-_]+#")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight namespace functions (current and other namespaces) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-resetable *pjs-namespace-functions-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-functions-regexp-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-variables-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-variables-regexp-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-classes-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-classes-regexp-cache* nil 'pjs-compile)

(defun list-pjs-namespace-functions-regexp (namespace)
  (unless *pjs-namespace-functions-regexp-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-functions-regexp-cache*))

(defun list-pjs-namespace-functions (namespace)
  (unless *pjs-namespace-functions-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-functions-cache*))

(defun list-pjs-namespace-variables-regexp (namespace)
  (unless *pjs-namespace-variables-regexp-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-variables-regexp-cache*))

(defun list-pjs-namespace-variables (namespace)
  (unless *pjs-namespace-variables-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-variables-cache*))

(defun list-pjs-namespace-classes-regexp (namespace)
  (unless *pjs-namespace-classes-regexp-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-classes-regexp-cache*))

(defun list-pjs-namespace-classes (namespace)
  (unless *pjs-namespace-classes-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-classes-cache*))

(defun search-pjs-current-namespace-functions (end)
  (let* ((namespace (pjs-current-namespace))
	 (ns-functions (list-pjs-namespace-functions-regexp namespace)))
    (re-real-search-forward (format "\\(?:%s\\.\\)?%s" namespace ns-functions) end t)))

(defun search-pjs-current-namespace-variables (end)
  (let* ((namespace (pjs-current-namespace))
	 (ns-variables (list-pjs-namespace-variables-regexp namespace)))
    (re-real-search-forward (format "\\(?:%s\\.\\)?%s" namespace ns-variables) end t)))

(defun search-pjs-current-namespace-classes (end)
  (let* ((namespace (pjs-current-namespace))
	 (ns-classes (list-pjs-namespace-classes-regexp namespace)))
    (re-real-search-forward (format "\\(?:%s\\.\\)?%s" namespace ns-classes) end t)))

(defun init-pjs-namespace-cache ()
  (unless (hash-table-p *pjs-namespace-functions-cache*)    
    (setq *pjs-namespace-functions-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-variables-cache*)    
    (setq *pjs-namespace-variables-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-classes-cache*)    
    (setq *pjs-namespace-classes-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-functions-regexp-cache*)    
    (setq *pjs-namespace-functions-regexp-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-variables-regexp-cache*)    
    (setq *pjs-namespace-variables-regexp-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-classes-regexp-cache*)    
    (setq *pjs-namespace-classes-regexp-cache* (make-hash-table :test 'equal)))    

  (when (fi::lep-open-connection-p)
    (dolist (ns (fi:eval-in-lisp "(jvs::get-all-namespace-members)"))
      (puthash (car ns) (js--regexp-opt-symbol (car (cdr ns)))    *pjs-namespace-variables-regexp-cache*)
      (puthash (car ns) (js--regexp-opt-symbol (second (cdr ns))) *pjs-namespace-functions-regexp-cache*)
      (puthash (car ns) (js--regexp-opt-symbol (third (cdr ns)))  *pjs-namespace-classes-regexp-cache*)
      (puthash (car ns) (car (cdr ns))    *pjs-namespace-variables-cache*)
      (puthash (car ns) (second (cdr ns)) *pjs-namespace-functions-cache*)
      (puthash (car ns) (third (cdr ns))  *pjs-namespace-classes-cache*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local vars based on grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pjs-semantic)

(defun pjs-start-of-next-block (tag) ;; (&optional (tag (semantic-current-tag)))
  (catch 'exit
    (let ((next-tag (semantic-find-tag-by-overlay-next)))
      (while next-tag
	(cond ((> (semantic-tag-start next-tag) (semantic-tag-end tag))
	       (throw 'exit (semantic-tag-end tag)))
	      ((eq (semantic-tag-class next-tag) 'block)
	       (throw 'exit (semantic-tag-start next-tag)))
	      (t
	       (setq next-tag (semantic-find-tag-by-overlay-next (semantic-tag-start next-tag)))))))
    (semantic-tag-end tag)))

(defun search-pjs-local-vars (end)
  (catch 'exit
    (while (< (point) end)
      (let ((current-tag (car (last (semantic-find-tag-by-overlay (point))))))
	(cond ((null current-tag)
	       (let ((next-tag (semantic-find-tag-by-overlay-next)))
		 (if next-tag
		     (goto-char (semantic-tag-start next-tag))
		   (throw 'exit nil))))
	      ((semantic-tag-p current-tag)	       
	       (let ((local-vars (semantic-get-local-variables))
		     ;; end is the start of the next block or the end of the current tag
		     (end (min end
			       (or (pjs-start-of-next-block current-tag)
				   (semantic-tag-end current-tag)))))
;;		 (message "local-vars %s" local-vars)
		 (when (re-real-search-forward (js--regexp-opt-symbol (mapcar 'car local-vars)) end t)
		   (throw 'exit (point)))
		 ;; we didn't find anything, go to the end
		 (goto-char end)))
	      (t
	       (message "nothing ??")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight kernel functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-resetable *pjs-kernel-functions-cache* nil 'pjs-compile)

(defvar *pjs-kernel-functions-present* t)

(defun list-pjs-kernel-functions ()
  (cond (*pjs-kernel-functions-cache*
	 *pjs-kernel-functions-cache*)
	(*pjs-kernel-functions-present*
	 (progn (setq *pjs-kernel-functions-present* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(if (fboundp 'jvs::list-all-js-functions) t nil)")))
		(when *pjs-kernel-functions-present*
		  (setq *pjs-kernel-functions-cache* (format "\\(\\(?:plw\\)?\\.%s\\)\\s-*(" (js--regexp-opt-symbol (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(jvs::list-all-js-functions)"))))))
		*pjs-kernel-functions-cache*))
	(t
	 nil)))

(defun search-pjs-kernel-functions (end)
  (when (list-pjs-kernel-functions)
    (let ((search-pattern (list-pjs-kernel-functions)))
      (re-real-search-forward search-pattern end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight method / members depending on variable type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-member-type (namespace class varname)
  (let ((ht (pjs-class-members namespace class)))    
    (when ht
      (gethash varname ht))))

(defun convert-pjs-type (type)
  (cond ((null type)
	 nil)
	((consp type)
	 type)
	((member type *pjs-standard-types*)
	 (cons "plw" type))
	(t
	 (let ((strings (split-string type "\\.")))
	   (cond ((= (length strings) 1)
		  (cons (pjs-current-namespace) (car strings)))
		 (t
		  (cons (car strings) (second strings))))))))

;; get the variable type from context :
;; if the variable is local to the function, search a var <type> varname =
;; TODO: if it is a global var of the namespace, check its type
;; TODO: if it is a member of a classe, check the type from the class.
;; we consider we are at the point between the var and its "father"
;; we return a cons (namespace . classname)
(defun get-variable-type-in-context (point )
  ;; use the parser
  (save-match-data
    (save-excursion
      (goto-char point)
      (when (re-search-backward (format "\\<%s\\>" *pjs-variable-name*) (line-beginning-position) t)
  	(let ((varname (downcase (match-string-no-properties 0)))  	      
	      var-tag)
  	  (cond ((looking-back "\\.")
  		 ;; we have a dot, try to get the type of the "father"
  		 (let ((father-type (get-variable-type-in-context (match-beginning 0))))
  		   (when father-type
  		     (get-member-type (car father-type) (cdr father-type) varname))))
		;; this of a method
		((string= (downcase varname) "this")
		 (let ((current-tag (car (semantic-find-tag-by-overlay))))
		   (when (and current-tag
			      (eq (semantic-tag-class current-tag) 'function))
		     (convert-pjs-type (semantic-tag-get-attribute current-tag :on-class)))))
  		;; local variable of the function, try to get the type
  		((setq var-tag (car (semantic-find-tags-by-name varname (semantic-something-to-tag-table (semantic-get-local-variables)))))
		 (convert-pjs-type (semantic-tag-get-attribute var-tag :type)))
  		;; member of a typed variable
  		()
  		;; namespace var with type
  		()
		(t
;;		 (message "nothing found :(")
		 nil)
		))))))
  

(defconst *pjs-vars-with-members-or-methods*
    (format "\\.\\<\\(%s\\)\\>" *pjs-variable-name*))
;;  (format "\\<\\(%s\\)\\>\\.\\<\\(%s\\)\\>\\(?:(\\)?" *pjs-variable-name* *pjs-variable-name*))

(defconst *pjs-standard-types*
  '("string"
    "boolean"
    "number"
    "vector"
    "array"
    "hashtable"))

(defun search-vars-with-members-or-methods (end)
  (let ((match-data (match-data))
	continue)
    (or (catch 'exit
	  (while (re-real-search-forward *pjs-vars-with-members-or-methods* end t)
	    (save-match-data
	      (let* ((member   (downcase (match-string-no-properties 1)))
		     (var-type (get-variable-type-in-context (match-beginning 0))))
		(when var-type
		  (let* ((members (pjs-class-members (car var-type) (cdr var-type)))
			 (methods (pjs-class-methods (car var-type) (cdr var-type))))
;;		    (message "member is %s members are %s methods are %s %s %s" member members methods (member member members) (member member methods))
		    (when (or (not (eq (gethash member members :unknown) :unknown))
			      (member member methods))
		      (throw 'exit (point)))))))))
	(progn (set-match-data nil)
	       nil))))

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
  ;;  (push (cons 'search-function-local-vars font-lock-variable-name-face) font-locks)
  (push (cons 'search-pjs-local-vars font-lock-variable-name-face) font-locks)

  ;; members and methods based on type of var
  (push (list 'search-vars-with-members-or-methods 1 opx2-hg-getset-face) font-locks)

  ;; Namespace vars
  (push (cons 'search-pjs-current-namespace-variables pjs-var-definition-face) font-locks)

  ;; Namespace classes
;  (push (cons 'search-pjs-current-namespace-classes font-lock-type-face) font-locks)

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
	 ;; simple arguments
	 (list *pjs-arguments-end*
	       '(progn (backward-char)
		       (min (or (ignore-errors (scan-sexps (point) 1))
				(line-end-position))
			    (line-end-position)))
	       '(re-search-backward "(" (line-beginning-position) t)
	       '(1 pjs-var-definition-face))
	 ;; arguments with types
	 (list *pjs-arguments-type-end*
	       '(progn (backward-char)
		       (min (or (ignore-errors (scan-sexps (point) 1))
				(line-end-position))
			    (line-end-position)))
	       '(end-of-line)
	       '(1 font-lock-type-face)
	       '(2 pjs-var-definition-face))
	 )
	font-locks)
  
  ;; Method definition
  (push (list *pjs-method-heading* 1 font-lock-function-name-face) font-locks)
  (push (list *pjs-method-heading* 2 font-lock-keyword-face) font-locks)
  (push (list *pjs-method-heading* 3 font-lock-type-face) font-locks)  
  ;; Method arguments
  (push (list
	  (concat *pjs-method-arguments-start*)
	  ;; simple arguments
	  (list *pjs-arguments-end*
		'(progn (backward-char)
			(min (or (ignore-errors (scan-sexps (point) 1))
				 (line-end-position))
			     (line-end-position)))
		'(re-search-backward "(" (line-beginning-position) t)
		'(1 pjs-var-definition-face))
	  ;; arguments with types
	  (list *pjs-arguments-type-end*
		'(progn (backward-char)
			(min (or (ignore-errors (scan-sexps (point) 1))
				 (line-end-position))
			     (line-end-position)))
		'(end-of-line)
		'(1 font-lock-type-face)
		'(2 pjs-var-definition-face))
	  )
	 font-locks)
  
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
	     nil
	     nil
	     ))

  (font-lock-mode)
  ;; build the cache before fontifying 
;;  (jit-lock-register 'build-local-vars-cache)

  ;; fontify all the things
  (syntax-propertize (point-max))
  )

(defun force-syntax-highlighting ()
  (interactive)
  (font-lock-fontify-buffer))

(defun pjs-reset-cache-on-save ()
  (js-reset-vars 'pjs-save))

(defun pjs-reset-cache-on-compile ()
  (js-reset-vars 'pjs-compile))
