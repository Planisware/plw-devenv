;;* 
;;  COPYRIGHT (C) PLANISWARE 2016-05-27
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************

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
