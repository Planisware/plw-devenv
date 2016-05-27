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
(require 'overlay) ;;needed by Xemacs, provided by the fsf-compat package

(defface plw-source-color-ok
  '((t (:background "honeydew")))
  "Face for displaying code which is actually defined in this patch"
  :group 'plw-source-color)

(defface plw-source-color-not-ok 
  '((t (:background "MistyRose1")))
  "Face for displaying code which has been redefined in another patch"
  :group 'plw-source-color)

(defvar plw-source-color-overlays nil "Overlays used in this buffer")
(defvar plw-source-color-available nil "Overlays available for reuse")

(make-variable-buffer-local 'plw-source-color-overlays)
(make-variable-buffer-local 'plw-source-color-available)


(defun plw-colorize-redef()
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (setq plw-source-color-available plw-source-color-overlays)
      (setq plw-source-color-overlays nil)
      (save-excursion
	(mapc #'plw-source-colorize
	      (get-buffer-window-list buffer nil 'visible)))
      (mapc #'delete-overlay plw-source-color-available)
      (setq plw-source-color-available nil))))

(defun plw-source-colorize(win)
  "Update colors for the portion visible in window"
  (when (fi::lep-open-connection-p)
    (let ((colors (fi:eval-in-lisp (format "(:emacs-source-file %S)" (buffer-file-name (window-buffer win))))))      
      (when colors	
	(dolist (color-spec colors)
	  (let* ((offset (with-current-buffer (window-buffer win) (point-min)))
		 (id (first color-spec))
		 (start (+ offset (second color-spec)))
		 (end (+ offset (third color-spec)))
		 (status (fourth color-spec))
		 (o (dolist (o (overlays-in start end) nil)
		      (when (equal (overlay-get o 'plw-colorize-source) id)
			(unless (memq o plw-source-color-overlays)
			  (push o plw-source-color-overlays))
			(setq plw-source-color-available 
			      (delq o plw-source-color-available))
			(return o)))))
	    ;;Start is the end of the previous sexp, adjust it
	    (goto-char start)
	    (fi:next-top-level-form)
	    (setq start (point))

	    (cond (o 
		   (move-overlay o start end))
		  (t
		   (setq o (make-overlay start end))
		   (push o plw-source-color-overlays)))
	    
	    (overlay-put o 'face (if status 'plw-source-color-ok 'plw-source-color-not-ok))
	    (overlay-put o 'plw-colorize-source id)))))))
      
;;(define-minor-mode plw-source-color-mode
;;  "Toggle display of source indication in the left margin area"
;; (cond (plw-source-color-mode
;;	 (add-hook 'after-change-functions 'plw-source-code-after-change))
;;	(t
;;	 (remove-hook 'after-change-functions 'plw-source-code-after-change t))))


	      

(defvar *enable-tutu-highlighting* nil)

;; apply
(when *enable-tutu-highlighting*
  (require 'cl)
  
  (defun %alisp-regexp-opt (list kind)
    "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
    (case kind
      (:word
       (concat "\\_<" (regexp-opt list t) "\\_>"))
      (:func
       (concat "(" "\\<" (regexp-opt list t) "\\>[ \\t\\n]"))))
  
;;;**************************
;;;   lisp face and regexp
;;;**************************
  
  (defgroup ophx2-alisp nil
    "Face for alisp highlighting"
    :group 'ophx2-alisp)
  
  (defface comment-tag-face
    '((t :foreground "green"))
    ""
    :group 'ophx2-alisp)
  
  (defvar comment-tag-face 'comment-tag-face)
  
  (defface opx2-hg-getset-face
    '((t :foreground "#5f9ea0"))
    ""
    :group 'ophx2-alisp)
  
  (defvar opx2-hg-getset-face 'opx2-hg-getset-face)
  
  (defface opx2-hg-tnil-face
    '((t :foreground "#7cfc00"))
    ""
    :group 'ophx2-alisp)
  
  (defvar opx2-hg-tnil-face 'opx2-hg-tnil-face)
  
  (defface opx2-hg-boolop-face
    '((t :foreground "#daa520"))
    ""
    :group 'ophx2-alisp)
  
  (defvar opx2-hg-boolop-face 'opx2-hg-boolop-face)
  
  (defconst opx2-hg-comment
    (%alisp-regexp-opt
     '("XXX" "TODO") :word))
  
  (defconst opx2-hg-alisp-tnil
    (%alisp-regexp-opt '("t" "nil") :word))
  
  (defconst opx2-hg-alisp-getset
    (%alisp-regexp-opt
     '("set" "get" "setf" "getf" "setq" "gethash" "aref") :func))
  
  (defconst opx2-hg-alisp-format
    (%alisp-regexp-opt '("format") :func))
  
  (defconst opx2-hg-alisp-boolop
    (%alisp-regexp-opt
     '("t" "nil" "null" "not" "and" "or" "eq" "equal" "string-equal" "=" "<" ">" "!=" "=<" "=>") :func))
  
  (setq highlight-rules-new
	`((,opx2-hg-comment        1 comment-tag-face t)
	  (,opx2-hg-alisp-getset   1 opx2-hg-getset-face)
	  (,opx2-hg-alisp-format   1 opx2-hg-getset-face)
	  (,opx2-hg-alisp-boolop   1 opx2-hg-boolop-face)
	  (,opx2-hg-alisp-tnil     1 opx2-hg-tnil-face)))
  
  
;;;*****************************
;;; lisp listener highlighting
;;;*****************************

  (defface lisp-shell-face
    '((t :foreground "#99CCFF"))
    ""
    :group 'ophx2-alisp)
  
  
  (defvar lisp-shell-face 'lisp-shell-face)
  
  
  (defface lisp-shell-tcp-face
    '((t :foreground "#66FF66"))
    ""
    :group 'ophx2-alisp)2
  (defvar lisp-shell-tcp-face 'lisp-shell-tcp-face)
  
  
  (defface lisp-shell-nb-face
    '((t :foreground "#cccc00"))
    ""
    :group 'ophx2-alisp)
  
  (defvar lisp-shell-nb-face 'lisp-shell-nb-face)
  
;  (setq %hl-shell-regexp "\\(\\(?:\\sw\\|-\\)+\\)(\\([0-9]+\\)):")
;  (setq %hl-shell-regexp "\\(\\sw+\\)(\\([0-9]+\\)):")
  (setq %hl-shell-regexp "\\(\\sw+\\(-\\sw+\\)?\\)(\\([0-9]+\\)):")
  (setq hl-shell-regexp (concatenate 'string "^" %hl-shell-regexp))
  (setq hl-shell-err-regexp (concatenate 'string "^\\(\[[0-9c]*\]\\) " %hl-shell-regexp))
  
  (setq lisp-shell-hg-rules
	`((,hl-shell-regexp      (1  lisp-shell-face) (3 lisp-shell-nb-face))
	  (,hl-shell-err-regexp  (1 font-lock-warning-face) (2  lisp-shell-face) (4 lisp-shell-nb-face))))
  
  (setq lisp-shell-tcp-hg-rules
	`((,hl-shell-regexp      (1  lisp-shell-tcp-face) (3 lisp-shell-nb-face))
	  (,hl-shell-err-regexp  (1 font-lock-warning-face) (2  lisp-shell-tcp-face) (4 lisp-shell-nb-face))))
  

  (font-lock-add-keywords 'fi:common-lisp-mode highlight-rules-new)
  (font-lock-add-keywords 'fi:inferior-common-lisp-mode highlight-rules-new)
  (font-lock-add-keywords 'fi:lisp-listener-mode highlight-rules-new)

  (font-lock-add-keywords 'fi:inferior-common-lisp-mode lisp-shell-hg-rules)
  (font-lock-add-keywords 'fi:lisp-listener-mode lisp-shell-tcp-hg-rules))

	 
