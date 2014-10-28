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
;;;; $Log$
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
