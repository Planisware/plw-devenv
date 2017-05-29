;; -*- coding: windows-1252 -*- 
;;  COPYRIGHT (C) PLANISWARE 2017
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************

;;; no stupid message about abbrevs
(setq save-abbrevs 'silently)

(defun fullpath-relative-to-current-file (file-relative-path)  
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-relative-path))

(defvar *javascript-evaluator-mode* :repl)
;; use opx2-js-mode
(defvar *use-opx2-js-mode* t)

;; packages taken from conf
(add-to-list 'load-path (fullpath-relative-to-current-file "packages"))
;; byte compile all packages we use
(byte-recompile-directory (fullpath-relative-to-current-file "packages") 0)

(load (fullpath-relative-to-current-file "el-ext/packages.el"))
(load (fullpath-relative-to-current-file "el-ext/common-plw.el"))
(load (fullpath-relative-to-current-file "el-ext/connect-is.el"))
(load (fullpath-relative-to-current-file "el-ext/source-code-plw.el"))
(load (fullpath-relative-to-current-file "el-ext/source-colorization.el"))
(load (fullpath-relative-to-current-file "el-ext/eli-plw.el"))


(when *use-opx2-js-mode*
  (load (fullpath-relative-to-current-file "el-ext/opx2-js-cache.el"))
  (load (fullpath-relative-to-current-file "el-ext/opx2-js-mode-syntax.el"))
  (load (fullpath-relative-to-current-file "el-ext/opx2-js-mode.el"))
  (load (fullpath-relative-to-current-file "el-ext/ac-opx2js.el"))

;;  (load (fullpath-relative-to-current-file "el-ext/pjs-semantic.el"))
  (load (fullpath-relative-to-current-file "el-ext/pjs-mode-syntax.el"))
  (load (fullpath-relative-to-current-file "el-ext/pjs-semantic.el"))
  (load (fullpath-relative-to-current-file "el-ext/ac-pjs.el"))
  (load (fullpath-relative-to-current-file "el-ext/pjs-mode.el"))
  ;;  (load (fullpath-relative-to-current-file "el-ext/yasnippet.el"))
  )

(load (fullpath-relative-to-current-file "el-scripts/ac-config.el"))

(when (eq *javascript-evaluator-mode* :repl)
  (load (fullpath-relative-to-current-file "el-scripts/javascript-evaluator.el")))

(defvar *custom-theme* 'tangotango)

;; dark theme
(when (and (>= emacs-major-version 24)
	   *custom-theme*)
  (add-to-list 'custom-theme-load-path (fullpath-relative-to-current-file "themes"))
  (add-to-list 'custom-theme-load-path (fullpath-relative-to-current-file "custom-themes"))
  (load-theme *custom-theme* t))


;;------------- general emacs configuration ----------------

;;split the window horizontally by default
(setq split-height-threshold 10)	; default is 80

;;parenthesis matching
(show-paren-mode 1)
(setq show-paren-delay 0) ;;show matching parenthesis immediatly

;; no sound when typing
(setq ring-bell-function 'ignore)

;;cursor configuration
(blink-cursor-mode 0) ;;no blinking curor !
;;(set-cursor-color "indian red") ;;light red

;;scrollbar on the right
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right) 

;;Ctrl+z = undo
(global-set-key [(control z)] 'undo)
;;Ctrl+tab : change winsdow
(global-set-key [(control tab)] 'other-window)

;; magit status 
(global-set-key (kbd "C-c C-g")    'magit-status)

;; do not check commit line length
(setq git-commit-finish-query-functions nil)

;;make sure emacs does not ask us to apply these parameters
(custom-set-variables
 '(safe-local-variable-values (quote ((Package . OPX2-USER) (Syntax . Common-Lisp)))))

;;use system clipboard
(setq x-select-enable-clipboard t)

(setq js-indent-level 2)

;;Ctrl+right mouse click = copy symbol/sexp
(defun mouse-get-thing (click)
  (interactive "e")
  (let ((str nil)
	(window (selected-window)))
    (save-current-buffer
      (save-excursion 
	(mouse-set-point click)
	(setq str (or (thing-at-point 'symbol)
		      (thing-at-point 'sexp)))
	(select-window window)))
    (when str
      (insert str))))

(global-unset-key [(control down-mouse-3)]) ;;to allow mouse-3 to work
(global-set-key [(control mouse-3)] 'mouse-get-thing)

;; use yasnippets
(when (require 'yasnippet nil 'noerror)

  (pushnew (fullpath-relative-to-current-file "el/yasnippet") yas-snippet-dirs :test 'string=)
  (yas-reload-all)

  ;; yasnippet configuration

  ;; only expand snippet after typing
  (setq yas-expand-only-for-last-commands '(self-insert-command))
  
  (add-hook 'prog-mode-hook #'yas-minor-mode))  
