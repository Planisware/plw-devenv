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

;;-------------------------- OJS customization -----------------------

(add-to-list 'auto-mode-alist '("\\.OJS\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ojs\\'" . c++-mode))

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
