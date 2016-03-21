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

;;;; Revision 3.10  2016/03/21 13:21:50  troche
;;;; * merge from git
;;;;
;;;; Revision 3.9  2015/11/06 10:23:33  troche
;;;; * proper lisp completion when you start with a package
;;;;
;;;; Revision 3.8  2015/06/18 08:32:28  troche
;;;; * configuration
;;;;
;;;; Revision 3.7  2015/05/20 15:10:05  mgautier
;;;; - always use minibuffer to display function signature after completion (for those using a 40 characters width buffer)
;;;;
;;;; Revision 3.6  2015/05/06 13:36:07  troche
;;;; * show arglist on completion
;;;;
;;;; Revision 3.5  2015/05/04 14:37:04  troche
;;;; * display the package only in the documentation
;;;;
;;;; Revision 3.4  2015/01/21 16:25:07  mgautier
;;;; - add filename completion in alisp's modes
;;;;
;;;; Revision 3.3  2015/01/06 17:03:37  troche
;;;; * update of the opx2 javascript mode with (almost) intelligent syntax highlighting and completion
;;;; * update of the javascript evaluator, now you don't exit it if you have a lisp error
;;;;
;;;; Revision 3.2  2014/12/01 09:54:25  mgautier
;;;; do not fail if aouto-complete is not installed
;;;;
;;;; Revision 3.1  2014/11/28 17:59:50  mgautier
;;;; add auto-complete for alisp
;;;;  (header added automatically)
;;;;

;;(when (fboundp :require-patch) (:require-patch ""))
(when (fboundp :set-source-info) (:set-source-info "$RCSfile$" :id "$Id$" :version "$Revision$" :date "$Date$"))
(when (fboundp :doc-patch) (:doc-patch "add auto-complete for alisp"))

(require 'auto-complete nil 'noerror)
;(require 'pos-tip)

(defvar *ac-current-candidates* nil)

(ac-define-source "alisp-functions"
  '((candidates . alisp-functions-ac-candidates)
    (document . alisp-functions-ac-document)
					;(summary . alisp-ac-summary)
					;(prefix . alixsp-ac-prefix)
    (action . alisp-function-ac-action)
    (symbol . "al")
    (match . alisp-ac-match)
    (requires . -1)))

(defvar *packages-to-ignore* '("MODULE" "CL-USER" "COMMON-LISP-USER"))

;; we return everything, because the list we got from the lisp is already filtered
(defun alisp-ac-match (string list)
  list)
  
(defun alisp-functions-ac-document (symbol)
  symbol)

(defun alisp-ac-prefix ()
  ""
  "")

(defun lisp-arglist-minibuffer (string)
  "see fi:lisp-arglist, always use minibufer"
  (interactive (fi::get-default-symbol "Arglist for" t t))
  (fi::make-request (lep::arglist-session :fspec string)
    ;; Normal continuation
    (() (what arglist)
     (progn
       (message "%s's arglist: %s" what arglist)
       (fi::note-background-reply)))
    ;; Error continuation
    ((string) (error)
     (progn
       (message "Cannot get the arglist of %s: %s" string error)
       (fi::note-background-reply)))))


(defun alisp-function-ac-action()
  ;; TODO : insert the symbol when needed
  ;; get the package : everything before the first :
  (save-excursion
    (save-match-data 
      (let* ((full-candidate (cdr (assoc candidate *ac-current-candidates*)))
	     (pos (string-match ":" full-candidate))
	     (package (when (and (numberp pos) (> pos 0)) (substring full-candidate 0 pos))))
	;; insert the package at the right position
	(when package 
	  (backward-char (length candidate))
	  ;; check that the package is not already there	  	  	  
	  (unless (equal (buffer-substring-no-properties (point) (min (+ (point) (length package) 1) (point-max)))
			 (format "%s:" package))
	    (insert package)
	    (insert "::"))
	  (lisp-arglist-minibuffer full-candidate))))))

(defun ac-candidate-< (c1 c2)
  (catch 'return
    (dolist (ignore *packages-to-ignore*)      
      (cond ((string-prefix-p ignore (cdr c1) t)
	     (throw 'return nil))
	    ((string-prefix-p ignore (cdr c2) t)
	     (throw 'return t))))
    (string< (cdr c1) (cdr c2))))
  

;; see eli/fi-lep.el in allegro lisp install directory
(defun alisp-functions-ac-candidates ()
  (interactive)
  ;; reset packages
  (setq *ac-current-candidates* nil)

  (with-local-quit
    (let* ((end (point))
	   xpackage real-beg
	   (beg (save-excursion
		  (backward-sexp 1)
		  (while (= (char-syntax (following-char)) ?\')
		    (forward-char 1))
		  (setq real-beg (point))
		  (let ((opoint (point)))
		    (if (re-search-forward ":?:" end t)
			(setq xpackage
			      (concat
			       ":"
			       (buffer-substring-no-properties opoint (match-beginning 0))))))
		  (point)))
	   (pattern (buffer-substring-no-properties beg end))
	   (functions-only (if (eq (char-after (1- real-beg)) ?\() t nil))
	   (downcase (and (eq ':upper fi::lisp-case-mode)
			  (not (fi::all-upper-case-p pattern))))
	   (xxalist (sort (fi::lisp-complete-1 pattern xpackage functions-only) 'ac-candidate-<))
	   temp
	   (package-override nil)
	   (xalist
	    (if (and xpackage (cdr xxalist))
		(fi::package-frob-completion-alist xxalist)
	      (if (and (not xpackage)
		       ;; current package of buffer is not the same as the
		       ;; single completion match
		       (null (cdr xxalist)) ;; only one
		       (setq temp (fi::extract-package-from-symbol
				   (cdr (car xxalist))))
		       (not
			(string= (fi::full-package-name
				  (or (fi::package) "cl-user"))
				 (fi::full-package-name temp))))
		  (progn
		    (setq package-override t)
		    xxalist)
		xxalist)))
	   (alist (if downcase
		      (mapcar 'fi::downcase-alist-elt xalist)
		    xalist))
	   (completion
	    (when alist
	      (let* ((xfull-package-name
		      (if (string= ":" xpackage)
			  "keyword"
			(when xpackage
			  (fi::full-package-name xpackage))))
		     (full-package-name
		      (when xfull-package-name
			(if downcase
			    (downcase xfull-package-name)
			  xfull-package-name))))
		(when (or full-package-name package-override)
		  (setq pattern
			(format "%s::%s" full-package-name pattern)))
		(try-completion pattern alist)))))
      (setq *ac-current-candidates* alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; configuration of the minor mode


;; definition of a new autocomplete mode
(defun alisp-setup-auto-complete-mode ()
  "Setup ac-lisp to be used with auto-complete-mode."
  (setq ac-sources '(ac-source-alisp-functions ac-source-filename))
  )

(add-hook 'fi:lisp-mode-hook 'alisp-setup-auto-complete-mode)
(add-hook 'fi:subprocess-mode-hook 'alisp-setup-auto-complete-mode)
