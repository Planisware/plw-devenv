;;;; -*- coding: windows-1252 -*-
;;;; COPYRIGHT (C) PLANISWARE $Date: 2015/12/22 15:47:49 $ 
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
;;;; FILE    : $RCSfile: opx2-js-mode-syntax.el,v $
;;;;
;;;; AUTHOR  : $Author: troche $
;;;;
;;;; VERSION : $Id: opx2-js-mode-syntax.el,v 3.13 2015/12/22 15:47:49 troche Exp $
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile: opx2-js-mode-syntax.el,v $" :id "$Id: opx2-js-mode-syntax.el,v 3.13 2015/12/22 15:47:49 troche Exp $" :version "$Revision: 3.13 $" :date "$Date: 2015/12/22 15:47:49 $ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (:require-patch "")
;;;; HISTORY :
;;;; $Log: opx2-js-mode-syntax.el,v $
;;;; Revision 3.13  2015/12/22 15:47:49  troche
;;;; * oops
;;;;
;;;; Revision 3.12  2015/12/22 15:47:25  troche
;;;; * deactivate
;;;;
;;;; Revision 3.11  2015/12/22 12:31:37  troche
;;;; * use get-local-function-environment
;;;;
;;;; Revision 3.10  2015/12/18 15:08:14  troche
;;;; * real-search is back (and working this time)
;;;;
;;;; Revision 3.9  2015/12/15 13:39:48  troche
;;;; * limit size of regexps in kernel functions
;;;;
;;;; Revision 3.8  2015/12/14 15:33:23  troche
;;;; * debug
;;;;
;;;; Revision 3.7  2015/12/14 10:41:58  troche
;;;; * debug local vars in function
;;;;
;;;; Revision 3.6  2015/06/18 08:32:28  troche
;;;; * configuration
;;;;
;;;; Revision 3.5  2015/05/12 12:39:25  troche
;;;; * debug
;;;;
;;;; Revision 3.4  2015/05/12 11:35:30  troche
;;;; * debug when the file is almost empty
;;;;
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
"^\\s-*function\\s-+\\(\\w+\\)"
"Regular expression matching the start of a function header.")

;; function arguments
(defconst *ojs-function-arguments-start*
  "\\<function\\>\\([ \t]+\\w+\\)?[ \t]*([ \t]*\\w")

;; function or method regexp
(defconst *ojs-function-or-method-regexp*
  "^\\s-*\\(?:\\<function\\>\\s-+\\([[:word:]_]+\\)\\|\\<method\\>\\s-+\\([[:word:]_]+\\)\\s-+\\<on\\>\\s-+\\([[:word:]_]+\\)\\)\\s-*([[:word:]_,]*)")

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
  "^\\<\\(class\\|function\\|method\\|on new\\|on modifyafter\\|on modifybefore\\|on delete\\)\\>.*{")

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

(defvar *regexp-elements-limit* 500)

(defun partition-list (list length)
  (loop
     while list
     collect (subseq list 0 (min (length list) length))
     do (setf list (nthcdr (min (length list) length) list))))

(defun list-ojs-kernel-functions ()  
  (cond (*ojs-kernel-functions-cache*
	 *ojs-kernel-functions-cache*)
	(*ojs-kernel-functions-present*
	 (setq *ojs-kernel-functions-cache*
	       (let ((functions-list (progn (setq *ojs-kernel-functions-present* (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(if (fboundp 'jvs::list-js-functions) t nil)")))
					    (sort (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(jvs::list-js-functions)")) 'string<)))
		     regexp-list)
		 
		 (dolist (sublist (partition-list functions-list *regexp-elements-limit*))
		   (push (format "\\(%s\\)" (js--regexp-opt-symbol sublist)) regexp-list))
		 regexp-list)))
	(t
	 nil)))

(defun search-kernel-functions (end)
  (let ((found end)
	(start (point)))
    (dolist (regexp (list-ojs-kernel-functions))
      (goto-char start)
      (setq found (or (re-search-forward regexp (or found end) t)
		      found)))
    (if (eq found end)
	nil
      found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight locally defined vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-function-local-vars (end)
  (search-vars-in-context end))

(defun list-local-vars-in-function ()
  (js--regexp-opt-symbol (get-local-vars-for-function nil)))

(defun search-vars-in-context (end)  
  ;;  ;;(message "Searching %s to %s" (point) end)
  (catch 'exit
    (while (< (point) end)      
      (let ((context (get-local-function-environment)))
;;	(search
	(cond ((numberp context)
	       (goto-char context))
	      ((consp context)
	       (let ((vars          (getf context :vars-regexp))
		     (end-of-fun    (getf context :end))
		     (next          (getf context :next)))
		 ;; search for the function vars in the context
		 ;; we catch something, return
		 (when (re-search-forward vars (min end-of-fun end) t)
		   (throw 'exit (point)))
		 ;; we didn't find anything, go to next function
		 ;; or exit if we don't have a next function
		 (goto-char (or (getf (get-local-function-environment next) :start) 
				(throw 'exit nil)))))
	      (t
	       (throw 'exit nil)))))))

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

(defvar *use-real-search* t)

;; like re-real-search-backward but searches text that is not
;; inside comments or strings
(defun re-real-search-backward (regexp limit errorp)
  (if *use-real-search*
      (let ((original-match-data (match-data))
	    (case-fold-search t) ;; case insensitive search
	    (orig-point (point))
	    (last-point (point))
	    ;; do a first search
	    (found-point (re-search-backward regexp limit errorp)))	
	;; checks that we are moving to avoid loops
	(while (and found-point
		    (< (point) last-point)
		    (or (er--point-is-in-comment-p)
			(er--point-is-in-string-p)))
	  (setq last-point found-point)
	  (setq found-point (re-search-backward regexp limit errorp)))
	(if found-point
	    found-point
	  ;; reset everything
	  (progn (goto-char orig-point)
		 (set-match-data original-match-data)
		 nil)))
    (re-search-backward regexp limit errorp)))

;; like re-search-forward but searches text that is not
;; inside comments or strings
(defun re-real-search-forward (regexp limit errorp)
  (if *use-real-search*
      (let ((original-match-data (match-data))
	    (case-fold-search t) ;; case insensitive search
	    (orig-point (point))
	    (last-point (point))
	    ;; do a first search
	    (found-point (re-search-forward regexp limit errorp)))
	;; checks that we are moving to avoid loops
	(while (and found-point
		    (> (point) last-point)
		    (or (er--point-is-in-comment-p)
			(er--point-is-in-string-p)))
	  (setq last-point found-point)
	  (setq found-point (re-search-forward regexp limit errorp)))
	(if found-point
	    found-point
	  ;; reset everything
	  (progn (goto-char orig-point)
		 (set-match-data original-match-data)
		 nil)))
    (re-search-forward regexp limit errorp)))

(defun function-boundaries ()
  ;; returns either a cons of (start end) containing the beginning and the end of the function
  ;; or nil if we are not in a function
  (save-excursion
    (let* ((start-point (point))
	   (function-start (re-search-backward *ojs-function-start-regexp* nil t)))
      (when function-start
	(while (and (not (looking-at "{"))
		    (< (point) (line-end-position)))
	  (forward-char))
	(when (looking-at "{")
	  (let ((function-end (or (condition-case nil
				      (progn (forward-list) (point))
				    ;; return nil when we have a scan error
				    (scan-error nil))
				  ;; try to find a lonely }, search only until the next start of function
				  (re-search-forward "^}$" (save-excursion (or (when (re-search-forward *ojs-function-start-regexp* nil t)
										 (line-beginning-position))
									       (point-max)))
						     t)
				  )))
	    (when (and function-end
		       (>= function-end start-point))
	      (cons function-start function-end))))))))
    

;; go to the start of the next function, but not after end
(defun goto-start-of-next-function (end)
  (re-search-forward *ojs-function-start-regexp* end t))

(defun inside-function ()
  (when (function-boundaries)
    t))

(defun get-local-vars-for-function (list-of-cons) 
  ;; if list-of-cons is t, we return a list of cons (variable . documentation)
  ;; if it is nil, we return a list of variable
  ;; the jit-lock-register function prepared a nice cache for us
  (let ((context (get-local-function-environment))
	res)
    (when context
      (maphash '(lambda (k v)
		  (if list-of-cons
		      (push (cons k (car v)) res)
		    (push k res)))
	       (getf context :vars))
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax hightlighting definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-ojs-syntax-highlighting ()
  ;; syntax highlighting for ojs
  
  (setq font-locks nil)

  (setq font-lock-verbose t)

  ;; list of font-lock-keywords in the right order

  ;; Functions defined in buffers
  (push (list 'search-buffer-functions 1 font-lock-function-name-face) font-locks)

  ;; Kernel functions
  (push (list 'search-kernel-functions 1 ojs-kernel-functions-face) font-locks)

  ;; Variables in the function 
  (push (cons 'search-function-local-vars font-lock-variable-name-face) font-locks)
  ;; Global vars
  (push (cons 'search-global-vars font-lock-variable-name-face) font-locks)
  ;; Variable definitions
  (push (list *ojs-vars-regexp* 1 ojs-var-definition-face) font-locks)

  ;; New type
  (push (list *ojs-new-type-regexp* 1 font-lock-type-face) font-locks)
  ;; Function definition
  (push (list *ojs-function-heading* 1 font-lock-function-name-face) font-locks)
  ;;  Function arguments
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

  (font-lock-mode)
  ;; build the cache before fontifying 
  (jit-lock-register 'build-local-vars-cache)  
  
  ;; regexp to mark the beginning of a function
;;  (setq defun-prompt-regexp *ojs-function-or-method-regexp*)

  ;; fontify all the things
  (syntax-propertize (point-max))
  )

(defun force-syntax-highlighting ()
  (interactive)
  (font-lock-fontify-buffer))	     
