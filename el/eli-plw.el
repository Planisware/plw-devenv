;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT


(defun revert-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))

(defun switch-to-listener ()
  (interactive)
  (let* ((buffer-name "*lisp-listener*")
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer)))
    (if (fi:process-running-p proc buffer-name)
	(fi::switch-to-buffer-new-screen buffer-name)
      (fi:menu-open-lisp-listener))))


(defun switch-to-common-lisp ()
  (interactive)
  (fi::switch-to-buffer-new-screen fi:common-lisp-buffer-name))

(global-set-key [f2] 'shell)
(global-set-key [f4] 'switch-to-listener)
(global-set-key [f5] 'switch-to-common-lisp)
(global-set-key [f6] 'delete-window)
(global-set-key [f11] 'revert-truncate-lines)

;; keyboard bindings

(defun indent-defun ()
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (fi:indent-sexp)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defvar *enable-lisp-completion* t)
(defvar *reverse-completion-shortcuts* nil)

(defun redefine-common-keys (map)
  (if *enable-lisp-completion*
      (cond (*reverse-completion-shortcuts*
	     (define-key map (kbd "<A-SPC>") 'fi:lisp-complete-symbol))
	    (t
	     (define-key map (kbd "<C-SPC>") 'fi:lisp-complete-symbol) ;;ctrl + space
	     (define-key map (kbd "<A-SPC>") 'set-mark-command))))
  ;; control c ! to list methods
  (define-key map "\C-c!" 'opx2-list-methods)
  ;;Make supr behave correctly in xemacs (works already this way in emacs)
  ;;(define-key map "\C-?" 'delete-char)
  )

(defun set-lisp-mode ()
  (auto-fill-mode -1) ;;Disable auto lines break
  
  ;; General shortcuts
  ;;(define-key fi:common-lisp-mode-map "C-M-q" 'indent-defun)
  (define-key fi:common-lisp-mode-map "\C-c\C-c" 'fi:lisp-compile-defun) ;;looks like it's not de the default

  (define-key fi:common-lisp-mode-map "\C-c!" 'opx2-list-methods)

  ;;'OPX2' CVS bindings (defined here but specific to emacs or xemacs and defined in main .el file)
  (define-key fi:common-lisp-mode-map "\C-oc" 'create-opx2)
  (define-key fi:common-lisp-mode-map "\C-oe" 'edit-opx2)
  (define-key fi:common-lisp-mode-map "\C-ou" 'unedit-opx2)

  (define-key fi:common-lisp-mode-map "\C-c:" 'uncomment-region)
  
;;  (define-key fi:common-lisp-mode-map "%" 'match-paren)

  ;;Common keys for all "lisp" modes
  (redefine-common-keys fi:common-lisp-mode-map))

(add-hook 'fi:common-lisp-mode-hook 'set-lisp-mode)
(add-hook 'fi:inferior-common-lisp-mode-hook (function (lambda() (redefine-common-keys fi:inferior-common-lisp-mode-map))))
(add-hook 'fi:lisp-listener-mode-hook (function (lambda() (redefine-common-keys fi:lisp-listener-mode-map))))

;;misc eli stuff

(setq fi:eli-compatibility-mode nil) ;;do not try to connect on 9666 (acl <= 6.2)

;; indentation rules
(put 'letf 'fi:common-lisp-indent-hook '(like flet))
(put 'letf 'fi:lisp-indent-hook '(like flet))

(put 'letnf 'fi:common-lisp-indent-hook '(like flet))
(put 'letnf 'fi:lisp-indent-hook '(like flet))

(put 'ol::letnf 'fi:common-lisp-indent-hook '(like flet))
(put 'ol::letnf 'fi:lisp-indent-hook '(like flet))


(put 'doplist 'fi:common-lisp-indent-hook '(like dolist))
(put 'doplist 'fi:lisp-indent-hook '(like dolist))

(put 'jvs::defun-js2 'fi:common-lisp-indent-hook 3)
(put 'defun-js2 'fi:common-lisp-indent-hook 3)
(put 'javascript::defun-js2 'fi:common-lisp-indent-hook 3)

;;  controversial
;;  (put 'if 'fi:common-lisp-indent-hook nil)
;;  (put 'if 'fi:lisp-indent-hook nil)

;; detect packages and methods better


(defun remove-package (str)
  (let ((start (or (and (string-match "[\\w_]*:+[\\w_]*" str)
			(match-end 0))
		   0)))
    (when (> start (length str))
      (setq start 0))
    (substring-no-properties str start)))

(defun get-package-at-point ()
  ;; try to find the package if present
  ;; back one symbol
  (save-excursion
    (ignore-errors
      (forward-sexp -1))
    (when (looking-at "\\sw+::\?\\sw+")
      (fi::defontify-string
       (buffer-substring (point)
			 (progn 
			   (while (not (looking-at ":"))
			     (forward-char 1))
			   (point)))))))

(defun get-method-symbol-at-point (&optional up-p)
  ;; do we have a defmethod ?
  (if (save-excursion
	(ignore-errors 
	  (forward-sexp -1)
	  (forward-sexp -1))
	(looking-at "defmethod"))
      (let* ((package (or (get-package-at-point)
			  (fi::package)))
	     (symbol (get-simple-symbol-at-point nil package))
	     start-class
	     class)
	(save-excursion
	  ;; get the class
	  (forward-sexp 1)	  
	  (while (looking-at " ")
	    (forward-char 1))
	  ;; we are at the start of the specifier
	  ;; we should have somethind like ((var class	    
	  (while (looking-at "[ (]")
	    (forward-char 1))
	  (forward-sexp 1)	    
	  (while (looking-at " ")
	    (forward-char 1))	    
	  (setq start-class (point))
	  (forward-sexp 1)
	  (setq class
		(remove-package
		 (fi::defontify-string
		  (buffer-substring
		   start-class
		   (point)))))
	  ;; do we have a package in our symbol ?
	  (cond (package
		 ;; try to find the "real" package of the defgeneric
		 (setq package (fi:eval-in-lisp (format "(opx2-lisp::find-real-package \"%s\" \"%s\")" symbol package)))
		 (format "%s::METHOD.%s.%s" (upcase package) (upcase symbol) (upcase class)))
		(t
		 (format "METHOD.%s.%s" (upcase symbol) (upcase class))))))
    (get-simple-symbol-at-point nil nil)))

;; comes from fi::get-symbol-at-point
;; redefined from fi-lep.el
(defun get-simple-symbol-at-point (&optional up-p package)
  (let* ((symbol
	  (cond
	   ((looking-at "\\sw\\|\\s_\\|[|()]")
	    (save-excursion
	      (while (looking-at "\\sw\\|\\s_\\|[|()]")
		(backward-char 1))
	      ;; advance until we have a "real" start of worf)
	      (while (not (looking-at "\\sw\\|\\s_"))
		(forward-char 1))
	      (fi::defontify-string
	       (buffer-substring-no-properties
		(point)
		(progn (forward-sexp 1)
		       ;; advance if we have a |
		       (when (looking-at "(")
			 (forward-sexp 1))
		       (when (looking-at "|")
			 (or (re-search-forward "|" (line-end-position) t 2)
			     (forward-sexp 1)))		       		       
		       (while (looking-at "\\s'")
			 (forward-char 1))
		       (point))))))		
	   (t
	    (condition-case ()
		(save-excursion
		  (if up-p
		      (let ((opoint (point)))
			(cond ((= (following-char) ?\()
			       (forward-char 1))
			      ((= (preceding-char) ?\))
			       (forward-char -1)))
			(up-list -1)
			(forward-char 1)
			(if (looking-at "def")
			    (goto-char opoint)
			  (if (looking-at "funcall\\|apply")
			      (progn
				(forward-sexp 2)
				(backward-sexp 1)
				(if (looking-at "#'")
				    (forward-char 2)
				  (if (looking-at "(function")
				      (progn
					(forward-char 1)
					(forward-sexp 2)
					(backward-sexp 1)))))))))
		  (while (looking-at "\\sw\\|\\s_")
		    (forward-char 1))
		  (if (re-search-backward "\\sw\\|\\s_" nil t)
		      (progn (forward-char 1)
			     (fi::defontify-string
			      (buffer-substring-no-properties
			       (point)
			       (progn (forward-sexp -1)
				      (while (looking-at "\\s'")
					(forward-char 1))
				      (point)))))
		    nil))
	      (error nil))))))
    (when (and symbol package) (setq symbol (remove-package symbol)))
    (when (and symbol (not package)) (setq symbol (fi::normalize-symbol-package symbol)))
    (or symbol
	(if (and up-p (null symbol))
	    (fi::get-symbol-at-point)))))

;; get the full symbol at point
;; ie the symbol
;; and if we are on a defmethod, get the method symbol
(defun fi::get-symbol-at-point (&optional up-p no-method)
  (if no-method
      (get-simple-symbol-at-point up-p nil)
    (get-method-symbol-at-point up-p)))

;; redefined from fi-utils.el
;; optional argument method?
(defun fi::get-default-symbol (prompt &optional up-p ignore-keywords no-method)
  ;;  (let ((symbol-at-point (fi::get-symbol-at-point up-p no-method)))
  (let ((symbol-at-point (fi::get-symbol-at-point up-p no-method)))
    (if fi::use-symbol-at-point
	(list symbol-at-point)
      (let ((read-symbol
	     (let ((fi::original-package (fi::package)))
	       (fi::ensure-minibuffer-visible)
	       (fi::completing-read
		(if symbol-at-point
		    (format "%s: (default %s) " prompt symbol-at-point)
		  (format "%s: " prompt))
		'fi::minibuffer-complete))))
	(list (if (string= read-symbol "")
		  symbol-at-point
		read-symbol))))))

(defun restart-emacs-lisp-connection ()
  (interactive)
  (unless (fi::lep-open-connection-p)
    (setq fi::*connection* nil)
    (fi::ensure-lep-connection)))

(defun add-restart-item ()
  (let ((map (cond
	      ((eq major-mode 'fi:common-lisp-mode) fi:common-lisp-mode-map)
	      ((eq major-mode 'fi:inferior-common-lisp-mode)
	       fi:inferior-common-lisp-mode-map)
	      ((eq major-mode 'fi:lisp-listener-mode)
	       fi:lisp-listener-mode-map)
	      (t (error "can't add these menus in this mode: %s"
			major-mode)))))   
    (define-key map [menu-bar acl restart-connection]
      (fi::menu "Restart Emacs / Common Lisp connection"
		'restart-emacs-lisp-connection
		:enable '(not (fi::connection-open))))))

(add-hook 'fi:inferior-common-lisp-mode-hook 'add-restart-item t)
(add-hook 'fi:common-lisp-mode-hook 'add-restart-item t)
(add-hook 'fi:lisp-listener-mode-hook 'add-restart-item t)

;; link to tpl:*prompt* redefine in devenv/dot-clinit.cl
(defvar *eli-prompt-package-regexp* "^\\(?:[\\[0-9c\\]*]\\)?\\([a-zA-Z0-9_]+:[0-9]+\\s-\\)?\\s-*\\([a-zA-Z-_]+\\)([0-9]+):")

(defun find-prompt-package ()
  (save-excursion
    (save-match-data      
      (when (re-search-backward *eli-prompt-package-regexp* (point-min) t)
	(match-string-no-properties 2)))))

(defun fi::package ()
  (let ((p (or (and (not fi::multiple-in-packages)
		    fi:package)
	       (save-excursion (fi::parse-package-from-buffer t t t))
	       (find-prompt-package))))
    (when p      
      (substring-no-properties p))))

;;; fix from eli/fi-emacs21.el
(defun fi::connection-open-composer-loaded ()
  (when (not (eq fi::*connection* fi::composer-cached-connection))
    ;; the lisp was (possibly) restarted
    (setq fi::connection-open-composer-loaded nil)
    (setq fi::composer-running nil))
  (and (fi::lep-open-connection-p)
       (or (when (or (null fi::connection-open-composer-loaded)
		     ;; check again, it might have been require'd
		     (eq fi::connection-open-composer-loaded 'no))
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp "(cl:when (cl:find-package :wt) cl:t)"))
		 (setq fi::connection-open-composer-loaded 'yes)
	       (setq fi::connection-open-composer-loaded 'no))
	     (setq fi::composer-cached-connection fi::*connection*)
	     nil)
	   (eq fi::connection-open-composer-loaded 'yes))))
