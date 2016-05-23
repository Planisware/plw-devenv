;;;;
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
;;;; (when (fboundp :require-patch) (:require-patch ""))
;;;; HISTORY :

;;;; Revision 3.3  2016/03/21 13:21:50  troche
;;;; * merge from git
;;;;
;;;; Revision 3.2  2014/10/28 12:57:56  troche
;;;; * New opx2 javascript emacs mode.
;;;; ** Add (defvar *use-opx2-js-mode* t) to your .emacs to use
;;;; * New opx2 javascript listener based on an emacs comint mode (still in testing).
;;;; ** Add (defvar *javascript-evaluator-mode* :comint) to your .emacs
;;;;
;;;; Revision 3.1  2011/07/27 07:09:26  folli
;;;; Moved some common xemacs/emacs functions in commonplw.el
;;;;  (header added automatically)
;;;;
;; -*-no-byte-compile: t; -*-

(defun on-ms-windows ()
  (memq system-type '(cygwin32 windows-nt ms-windows ms-dos win386)))

;;Colors
(custom-set-faces
 '(font-lock-string-face ((((class color) (background light)) (:foreground "green3"))))
 '(font-lock-preprocessor-face ((((class color) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "red"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "blue"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "orange")))))

;;general emacs config
(custom-set-variables
 '(column-number-mode t) ;;show line & col
 '(line-number-mode t))

;;undo is ctrl+z
(global-set-key [(control z)] 'undo)

;; Automagically add a LF at the end of a file.
(setq require-final-newline t)

(defun two-char-indent-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq c-indent-level 2))

;;2 spaces indent
(add-hook 'c-mode-hook 'two-char-indent-mode-hook)
(add-hook 'c++-mode-hook 'two-char-indent-mode-hook)
(add-hook 'java-mode-hook 'two-char-indent-mode-hook)
(add-hook 'javascript-mode-hook 'two-char-indent-mode-hook)
(add-hook 'opx2-js-mode-hook 'two-char-indent-mode-hook)

;;-------------------------- OJS customization -----------------------

(if *use-opx2-js-mode*
    (progn 
      (add-to-list 'auto-mode-alist '("\\.OJS\\'" . opx2-js-mode))
      (add-to-list 'auto-mode-alist '("\\.ojs\\'" . opx2-js-mode)))
  (progn 
    (add-to-list 'auto-mode-alist '("\\.OJS\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.ojs\\'" . c++-mode))))

;; <ctrl-c .> in ojs file
(defun ojs-find-definition(tag)
    (interactive
     (if current-prefix-arg
	 '(nil)
       (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
    (fi::lisp-find-definition-common (concat "js::" tag) nil))

(defun set-ojs-mode-hook()
  (define-key c++-mode-map "\C-c." 'ojs-find-definition))

(add-hook 'c++-mode-hook 'set-ojs-mode-hook)


(defvar *js-vars-to-reset* nil)

(defmacro defvar-resetable (varname def when &optional local)
  (unless (hash-table-p *js-vars-to-reset*)
    (setq *js-vars-to-reset* (make-hash-table :test 'eq)))
  `(progn
     (dolist (w (if (consp ,when) ,when (list ,when)))
       (pushnew ',varname (gethash w *js-vars-to-reset*)))
     ,(if local
	  `(defvar-local ,varname ,def)
	`(defvar ,varname ,def))))

;; global functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; like looking back but only on str, not on regexp
(defun fast-looking-back (str)
  (let ((point (point))
	(lstr (length str)))	
  (catch 'ret
    (do* ((i 0 (1+ i)))
	((= i lstr)
	 t)
      (let ((c (aref str (- lstr i 1)))
	    (char (char-before (- point i))))
	(unless (eq char c)
	  (throw 'ret nil)))))))

(defun fast-looking-at (str)
  (let ((point (point))
	(lstr (length str)))	
  (catch 'ret
    (do* ((i 0 (1+ i)))
	((= i lstr)
	 t)
      (let ((c (aref str i))
	    (char (char-after (+ point i))))
	(unless (eq char c)
	  (throw 'ret nil)))))))

(defun check-fixes-configuration (fixes-list)
  (catch 'exit
    (dolist (fix *pjs-required-fixes* t)
      (cond ((consp fix)
	     (when (fi::lep-open-connection-p)
	       (unless (fi:eval-in-lisp "(let ((fix (object::get-object 'object::fix \"%s\"))) (if (and fix (or (string= (object::fix-version fix) \"$%s\$\") (object::version>= (object::fix-version fix) \"%s\"))) t nil))" (car fix) "Revision" (second fix))
		 (message "Fix %s version %s not found, some functionalities have been disabled." (car fix) (second fix))
		 (throw 'exit nil))))
	    ((stringp fix)
	     (when (fi::lep-open-connection-p)
	       (unless (fi:eval-in-lisp "(let ((fix (object::get-object 'object::fix \"%s\"))) (if fix t nil))" fix)
		 (message "Fix %s not found, some functionalities have been disabled." fix)
		 (throw 'exit nil))))))))


;; searches only in non string and non comments 

(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun er--point-is-in-string-p ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defvar *use-real-search* t)

(defun re-search-forward-with-test (regexp test limit errorp)
  (%re-search-with-test 're-search-forward regexp test limit errorp))

(defun re-search-backward-with-test (regexp test limit errorp)
  (%re-search-with-test 're-search-backward regexp test limit errorp))

(defun %re-search-with-test (search-function regexp test limit errorp)
  (let ((original-match-data (match-data))
	(case-fold-search t) ;; case insensitive search
	(orig-point (point))
	(last-point (point))
	;; do a first search
	(found-point (funcall search-function regexp limit errorp)))	
    ;; checks that we are moving to avoid loops
    (while (and found-point
		(not (equal (point) last-point))
		(not (funcall test)))
      (setq last-point found-point)
      (setq found-point (funcall search-function regexp limit errorp)))
    (if found-point
	found-point
      ;; reset everything
      (progn (goto-char orig-point)
	     (set-match-data original-match-data)
	     nil))))

;; like re-real-search-backward but searches text that is not
;; inside comments or strings
(defun re-real-search-backward (regexp limit errorp)
  (if *use-real-search*
      (re-search-backward-with-test regexp
				    (lambda () (not (or (er--point-is-in-comment-p)
							(er--point-is-in-string-p))))
				    limit errorp)
    (re-search-backward regexp limit errorp)))

;; like re-search-forward but searches text that is not
;; inside comments or strings
(defun re-real-search-forward (regexp limit errorp)
  (if *use-real-search*
      (re-search-forward-with-test regexp
				    (lambda () (not (or (er--point-is-in-comment-p)
							(er--point-is-in-string-p))))
				    limit errorp)
    (re-search-forward regexp limit errorp)))


;;; Header checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *copyright-strings-to-replace* '(("__MODE__" mode)
					 ("__FILENAME__" filename)))

(defun check-header (&optional force)
  (interactive)
  (let ((head *script-copyright-head*))
    (when (or force
	      (and (not (string= (buffer-substring-no-properties 1 (min (point-max) (1+ (length head)))) head))
		   (y-or-n-p "Your copyright header seems absent or corrupted. Do you want to add or repair it ?")))
      (save-excursion
	(let ((copyright *script-copyright*)
	      (mode (cond ((eq major-mode 'pjs-mode) "pjs")
			  ((eq major-mode 'opx2-js-mode) "opx2-js")
			  (t "C++")))
	      (filename (file-name-nondirectory (buffer-file-name))))
	  (goto-char (point-min))	
	  ;; try to remove existing headers first
	  (when (or (looking-at "/+\\*")
		    (looking-at "^$"))
	    (while (and (or (looking-at "/+")
			    (looking-at "^$"))
			(not (looking-at "/+\\*\\{10,\\}")))
	      (forward-line))
	    (delete-region (point-min) (line-end-position)))
	  (dolist (replacement *copyright-strings-to-replace*)
	    (setq copyright (replace-regexp-in-string (first replacement) (symbol-value (second replacement)) copyright t)))
	  (insert copyright))))))
