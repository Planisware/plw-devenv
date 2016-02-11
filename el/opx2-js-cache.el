;;;; -*- coding: windows-1252 -*-
;;;; COPYRIGHT (C) PLANISWARE $Date: 2016/02/04 16:55:35 $ 
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
;;;; FILE    : $RCSfile: opx2-js-cache.el,v $
;;;;
;;;; AUTHOR  : $Author: troche $
;;;;
;;;; VERSION : $Id: opx2-js-cache.el,v 3.10 2016/02/04 16:55:35 troche Exp $
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile: opx2-js-cache.el,v $" :id "$Id: opx2-js-cache.el,v 3.10 2016/02/04 16:55:35 troche Exp $" :version "$Revision: 3.10 $" :date "$Date: 2016/02/04 16:55:35 $ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (:require-patch "")
;;;; HISTORY :
;;;; $Log: opx2-js-cache.el,v $
;;;; Revision 3.10  2016/02/04 16:55:35  troche
;;;; * don't use real search when not needed
;;;;
;;;; Revision 3.9  2015/12/22 12:31:09  troche
;;;; * build local cache before syntax highlighting
;;;;
;;;; Revision 3.8  2015/12/18 15:08:46  troche
;;;; * use real-search
;;;;
;;;; Revision 3.7  2015/12/15 13:39:11  troche
;;;; * debug
;;;;
;;;; Revision 3.6  2015/12/14 12:13:59  troche
;;;; * only highlight functions names followed by a (
;;;;
;;;; Revision 3.5  2015/12/14 10:41:09  troche
;;;; * pjs class members cache
;;;;
;;;; Revision 3.4  2015/06/18 08:32:28  troche
;;;; * configuration
;;;;
;;;; Revision 3.3  2015/05/06 14:31:51  troche
;;;; * do not match variables with the same name as a function
;;;;
;;;; Revision 3.2  2015/01/06 17:03:37  troche
;;;; * update of the opx2 javascript mode with (almost) intelligent syntax highlighting and completion
;;;; * update of the javascript evaluator, now you don't exit it if you have a lisp error
;;;;
;;;; Revision 3.1  2014/12/30 12:06:42  troche
;;;;  OPX2 javascript mode cache
;;;;  (header added automatically)
;;;;

;;;;; global cache for function and methods definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *js-variable-name*
  "[_a-zA-Z][_a-zA-Z0-9]*")

(defconst *js-function-name*
  *js-variable-name*)

(defconst *js-class-name*
  *js-variable-name*)

(defconst *js-namespace-name*
  *js-variable-name*)

(defconst *js-type*
  (format "\\(?:%s\\.\\)?%s" *js-namespace-name* *js-variable-name*))

(defconst *js-function-name-with-namespace*
  (format "%s\\.%s" *js-namespace-name* *js-function-name*))

(defconst *ojs-function-method-definition-regexp* 
  (format "^\\s-*\\(?:function\\|method\\)\\s-+\\(\\%s\\).*$" *js-function-name*))

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar *ojs-buffers-functions-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *ojs-buffers-functions-cache-regexp* nil)

(defun ojs-functions-in-buffers ()
  (or *ojs-buffers-functions-cache*
      (progn (generate-ojs-functions-cache)
	     *ojs-buffers-functions-cache*)))

(defun ojs-functions-in-buffers-regexp ()
  (or *ojs-buffers-functions-cache-regexp*
      (progn (generate-ojs-functions-cache)
	     *ojs-buffers-functions-cache-regexp*)))

(defun generate-ojs-functions-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-functions-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-function-method-definition-regexp*))
  ;; regexp cache
  (setq *ojs-buffers-functions-cache-regexp*
	(format "\\(%s\\)(" (js--regexp-opt-symbol (loop for item in *ojs-buffers-functions-cache*
							 collect (car item))))))
;;;;; class members/methods in ojs2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *pjs-namespace-definition*
  (format "^\\s-*namespace\\s-+\\(\\%s\\);" *js-namespace-name*))

;; return the current namespace 
(defun pjs-current-namespace ()
  (save-excursion
    (let* ((found (re-real-search-backward *pjs-namespace-definition* nil t))
	   (namespace (when found (match-string-no-properties 1))))
      (if found namespace "plw"))))  

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar *pjs-buffers-class-members-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *pjs-buffers-class-members-cache-regexp* nil)

;; used for autocomplete 
(defvar *pjs-buffers-class-methods-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *pjs-buffers-class-methods-cache-regexp* nil)

(defconst *pjs-class-declaration-regexp*
  (format "^\\s-*class\\s-+\\(\\%s\\)\\s-*{" *js-class-name*))

(defun pjs-class-members (class-name namespace)
  (unless (hash-table-p *pjs-buffers-class-members-cache*)
    (setq *pjs-buffers-class-members-cache* (make-hash-table :test 'equal)))
  (let ((type (format "%s.%s" namespace class-name)))
    (or (gethash type *pjs-buffers-class-members-cache*)
	(puthash type (fi:eval-in-lisp (format "(when (fboundp 'jvs::get-pjs-class-members) (jvs::get-pjs-class-members \"%s\" \"%s\"))" class-name namespace))
		 *pjs-buffers-class-members-cache*))))

(defun pjs-class-members-regexp (class-name namespace)
  (js--regexp-opt-symbol (pjs-class-members class-name namespace)))

(defun pjs-class-methods (class-name namespace)
  (unless (hash-table-p *pjs-buffers-class-methods-cache*)
    (setq *pjs-buffers-class-methods-cache* (make-hash-table :test 'equal)))
  (let ((type (format "%s.%s" namespace class-name)))
    (or (gethash type *pjs-buffers-class-methods-cache*)
	(puthash type (fi:eval-in-lisp (format "(when (fboundp 'jvs::get-method-for-js-class) (jvs::get-method-for-js-class \"%s\" \"%s\"))" class-name namespace))
		 *pjs-buffers-class-methods-cache*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lock-register function : we build a small cache with
;; all the variables in the functions defined from start to end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *js-local-vars-cache* nil)

(defconst *js-var-with-type-regexp* 
  (format "\\<var\\>\\s-+\\(\\<%s\\>\\)?\\s-*\\(\\<%s\\>\\)\\s-*\\(?:;\\|=.+\\|in.*\\)" *js-type* *js-variable-name*))

(defconst *js-function-method-header* 
  (format "\\<\\(?:function\\|method\\)\\>\\s-+\\(%s\\)\\(?:\\s-+on\\s-+\\(%s\\)\\)?\\s-*(" *js-function-name* *js-type*))

(defun build-local-vars-cache (start end)
  (let (res)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(let ((boundaries (function-boundaries))
	      (vars-ht (make-hash-table :test 'equal)))
	  (cond (boundaries
		 ;; we are in a function, search for vars
		 ;; first, get the arguments of the function
		 (let* ((start-function (goto-char (car boundaries)))
			(function-name (when (re-search-forward *js-function-method-header* (line-end-position) t)
					 ;; go back one char to be on the (, which the end of our regexp
					 (forward-char -1)
					 (match-string-no-properties 1)))
			(class-name    (match-string-no-properties 2))
			(begin-args (progn (while (and (not (looking-at "("))
						       (< (point) (line-end-position)))
					     (forward-char))
					   (if (looking-at "(") (point) nil)))
			(end-args (progn (while (and (not (looking-at ")"))
						     (< (point) (line-end-position)))
					   (forward-char))
					 (if (looking-at ")") (point) nil)))
			(begin-function (progn (while (and (not (looking-at "{"))
							   (< (point) (line-end-position)))
						 (forward-char))
					       (if (looking-at "{") (point) nil)))
			(end-function (or (cdr boundaries)
					  (point-max)))
			(function-line (when end-args (buffer-substring-no-properties start-function (1+ end-args) ))))
		   ;; function / methods arguments
		   (when (and function-line begin-args end-args)
		     (dolist (arg (split-string (buffer-substring-no-properties (1+ begin-args) end-args) "[ \t,]+" t))
		       ;; if the args are separated by spaces, we have a type arg
		       (let ((type-and-arg (split-string arg)))
			 (puthash (downcase (car type-and-arg))
				  (list function-line (second type-and-arg))
				  vars-ht)))
		     (when (and begin-function end-function)
		       (goto-char begin-function)
		       (while (re-search-forward *js-var-with-type-regexp* end-function t)
			 (puthash (downcase (match-string-no-properties 2))
				  (list (match-string-no-properties 0)
					(match-string-no-properties 1))
				  vars-ht)))
		     (push (list :function function-name :class class-name :start begin-function :end end-function :vars vars-ht) res))
		   (goto-char (1+ end-function))))
		((goto-start-of-next-function end)
		 t)
		(t
		 (goto-char (1+ end)))))))
    (setq *js-local-vars-cache* (reverse res))))

;; returns the local function environment around point
(defun get-local-function-environment ()
  (catch 'exit
    (dolist (context *js-local-vars-cache*)
      (cond ((and (>= (point) (getf context :start))
		  (<= (point) (getf context :end)))
	     ;; we found the context, yay	       
	     (throw 'exit context))
	    ((< (point) (getf context :start))
	     ;; too late, we exit
	     (throw 'exit nil))))))

;;;;; buffer dependant cache for script-level var definitions and global vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-file-vars-regexp*  
  "^var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")

(defconst *pjs-file-vars-regexp*
  "^var\\s-*\\(?:[[:word:].]+\\)?\\s-+\\(\\w+\\)\\s-*[=;].*$")
;;  "^var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")

(defconst *ojs-global-vars-regexp*  
  "^\\s-*global\\s-+var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")

;; used for autocomplete 
(defvar *ojs-buffers-vars-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *ojs-buffers-vars-cache-regexp* nil)

(defun ojs-vars-in-buffer ()
  (interactive)
  (or *ojs-buffers-vars-cache*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache*)))

(defun ojs-vars-in-buffer-regexp ()
  (or *ojs-buffers-vars-cache-regexp*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache-regexp*)))

(defun generate-ojs-vars-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-vars-cache* (append 
				  (ojs-find-candidates-from-regexp (if (eq major-mode 'pjs-mode)
								       *pjs-file-vars-regexp*
								     *ojs-file-vars-regexp*))
				  (when (eq major-mode 'opx2-js-mode)
				    (ojs-find-candidates-from-regexp-in-buffers *ojs-global-vars-regexp*))))
  ;; regexp cache
  (setq *ojs-buffers-vars-cache-regexp*
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-vars-cache*
				     collect (car item))))
  )

;;;;; global cache for kernel function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; cache reset on file save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ojs-reset-cache ()
  (setq *ojs-buffers-functions-cache* nil)
  (setq	*ojs-buffers-functions-cache-regexp* nil)
  (setq *ojs-buffers-vars-cache* nil)
  (setq *ojs-buffers-vars-cache-regexp* nil))

;;;;; generic function to find candidates based on a regexp 
;;;;; it returns a candidates list containing cons (name . documentation) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ojs-find-candidates-from-regexp (regexp &optional already-matched)
  (let (candidate
	candidates
	(point (point))
	)
    (save-excursion
      ;; Search forward from the line after the current one
      (forward-line 1)
      (while  (re-real-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; search backward from the start of the current line
      (goto-char point)
      (beginning-of-line)
      (while  (re-real-search-backward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; Search forward
      (nreverse candidates))))

(defun ojs-find-candidates-from-regexp-in-buffers (regexp )
  (let (candidates)
    (dolist (buffer (buffer-list))
      (when (or (derived-mode-p (buffer-local-value 'major-mode buffer))
		(equal (buffer-local-value 'major-mode buffer) 'opx2-js-mode))
	(with-current-buffer buffer
	  (let ((found (ojs-find-candidates-from-regexp regexp candidates)))
	    (setq candidates (append candidates found))))))
    candidates))
