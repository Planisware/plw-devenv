;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT


(require 'cc-mode)

(defvar pjs-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)
    ;;    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?# "'q" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table used in JavaScript mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu and keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pjs-mode-map* (make-sparse-keymap))

;; we don't want to stack definition declarations
(setq fi:maintain-definition-stack nil)

(defvar *pjs-required-fixes* '("sc8384"
			       ("sc9213" "3.44")))

(defvar-resetable *pjs-configuration-status* nil 'pjs-reset)

;; check that we have the proper configuration
(defun pjs-configuration-ok ()
  (unless *pjs-configuration-status*
    (setq *pjs-configuration-status*
	  (if (check-fixes-configuration *pjs-required-fixes*)
	      :ok :ko)))
  (cond ((and (eq *pjs-configuration-status* :ok)
	      (fi::lep-open-connection-p))
	 t)
	(t
	 nil)))

;; try to get a full symbol from the position given
(defun find-symbol-at-point (start end)
  ;; is the previous char a : ?
  (save-excursion
    (goto-char start)
    (backward-char)
    (when (fast-looking-at ":")
      (while (and (not (or (looking-at "[ (]")
			   (looking-at "^")))
		  (> (point) (point-min)))
	(backward-char))
      (if (looking-at "^")
	  (setq start (point))
	(setq start (1+ (point))))))
  (buffer-substring-no-properties start end))

;; adapted from fi::get-default-symbol
(defun pjs-get-default-symbol (prompt up-p no-method)
  (let* ((symbol-at-point (fi::get-symbol-at-point up-p no-method))
	 (function-at-point (if (string-match ":" symbol-at-point)
				symbol-at-point
			      (_find-function-at-point))))
    (if fi::use-symbol-at-point
	(list function-at-point)
      (let ((read-symbol
	     (let ((fi::original-package (fi::package)))
	       (fi::ensure-minibuffer-visible)
	       (fi::completing-read
		(if function-at-point
		    (format "%s: (default %s) " prompt function-at-point)
		  (format "%s: " prompt))
		'fi::minibuffer-complete))))
	(list (if (string= read-symbol "")
		  function-at-point
		read-symbol))))))

;; return the lisp symbol
(defun _find-function-at-point ()
  (save-excursion
    (let ((word (thing-at-point 'word t)) type)
      (cond ((or (string-prefix-p "plw" word t)
		 (string-match (list-pjs-namespaces-regexp) word)) ;; we are looking at the namespace
	     (format "%s.%s" word (progn (forward-word (if (fast-looking-at ".") 1 2))
					 (thing-at-point 'word t))))
	    (t
	     (beginning-of-thing 'word)
	     (cond ((fast-looking-back "plw.")
		    word)
		   ;;		    (format "plw.%s" word))
		   ((looking-back (format "%s\\." (list-pjs-namespaces-regexp)) (line-beginning-position))
		    (backward-word)
		    (format "%s.%s" (thing-at-point 'word t) word))
		   ((and (fast-looking-back ".")
			 (setq type (get-variable-type-in-context (point))))		    
		    (format "method.%s.%s" word
			    (if (or (string= (car type) "plc")
				    (string= (car type) "plw"))
				(or (and (list-pjs-plc-types-to-kernel)
					 (gethash (cdr type) (list-pjs-plc-types-to-kernel)))
				    (cdr type))
			      (format "%s.%s" (car type) (cdr type)))))
		   ((fast-looking-back ".")
		    word)
		   ;;		    (format "plw.%s" word))
		   ;; method definition
		   ((save-excursion
		      (beginning-of-line)
		      (re-search-forward *pjs-method-heading* (line-end-position) t))
		    (setq type (convert-pjs-type (match-string-no-properties 3)))
		    (format "method.%s.%s"
			    (match-string-no-properties 1)
			    (if (or (string= (car type) "plc")
				    (string= (car type) "plw"))
				(or (and (list-pjs-plc-types-to-kernel)
					 (gethash (cdr type) (list-pjs-plc-types-to-kernel)))
				    (cdr type))
			      (format "%s.%s" (car type) (cdr type)))))
		   (t
		    (format "%s.%s" (pjs-current-namespace) word))))))))

;; <ctrl-c .> in ojs file
;; works with ojs and lisp file and properly do the search
(defun %pjs-find-definition (tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (pjs-get-default-symbol "Lisp locate source" t t)))))
  ;;  (interactive)
  (if (string-match ":" tag)
      (fi::lisp-find-definition-common tag nil)
    (fi::lisp-find-definition-common (if (string-prefix-p "plw." tag t)
					 (concat "js::" (substring tag (1+ (position ?. tag))))
				       (concat "js::" tag)) nil)))

(defun %pjs-list-who-calls (tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (pjs-get-default-symbol "List who calls" t t)))))
  (if (string-match ":" tag)
      (fi:list-who-calls tag)
    (fi:list-who-calls (if (string-prefix-p "plw." tag t)
			   (concat "js::" (substring tag (1+ (position ?. tag))))
			 (concat "js::" tag)))))

(defvar *pjs-compilation-buffer-name* "*PJS compilation traces*")

(defun trace-pjs-function(tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (pjs-get-default-symbol "Lisp (un)trace function" t t)))))
  (when tag
    (if (string-match ":" tag)
	(fi:toggle-trace-definition tag)
      (fi:toggle-trace-definition (if (string-prefix-p "plw." tag t)
				      (concat "js::" (substring tag (1+ (position ?. tag))))
				    (concat "js::" tag))))))

(defun pjs-save-buffer ()
  (interactive)
  (save-buffer)
  (semantic-force-refresh))

(defun save-and-compile-pjs-file ()
  (interactive)
  (save-and-compile-ojs-file)
  (semantic-force-refresh))

(defun compile-pjs-file ()
  (interactive)
  (compile-ojs-file)
  (semantic-force-refresh))

(defun check-pjs-region ()
  (interactive)
  (compile-ojs-region)
  (semantic-force-refresh))

(defun pjs-reset-cache-on-reset ()
  (interactive)
  (js-reset-vars 'pjs-reset)
  (js-reset-vars 'ojs-reset)
  (js-reset-vars 'pjs-compile)
  (js-reset-vars 'ojs-compile)
  (js-reset-vars 'pjs-save)
  (js-reset-vars 'ojs-save)

  (when (buffer-file-name)
    (save-buffer)
    (plw-refresh-file-buffer (buffer-file-name))
    (semantic-force-refresh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js mode
(require 'js)

;; taken from js--proper-indentation from js-mode
(defun pjs--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
;;          ((eq (char-after) ?#) 0) ;; # is a normal char in pjs
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\s-*:\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (cond (same-indent-p
                                  (current-column))
                                 (continued-expr-p
                                  (+ (current-column) (* 2 js-indent-level)
                                     js-expr-indent-offset))
                                 (t
                                  (+ (current-column) js-indent-level
                                     (pcase (char-after (nth 1 parse-status))
                                       (?\( js-paren-indent-offset)
                                       (?\[ js-square-indent-offset)
                                       (?\{ js-curly-indent-offset)))))))
                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

;; taken from js-indent-line from js-mode
(defun pjs-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (indent-line-to (pjs--proper-indentation parse-status))
    (when (> offset 0) (forward-char offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-derived-mode opx2-js-mode prog-mode "OPX2 javascript"
(define-derived-mode pjs-mode prog-mode "Planisware Script V2"
  :syntax-table pjs-mode-syntax-table

  ;; load a little bit of cc-mode for indentation
  ;;  (c-initialize-cc-mode t)
  ;;  (c-init-language-vars c-mode)
  ;;  (c-init-language-vars-for 'pjs-mode)
  ;;  (c-common-init 'c-mode)

  ;; indentation like js mode
  (setq-local indent-line-function 'pjs-indent-line)
  (setq-local beginning-of-defun-function 'js-beginning-of-defun)
  (setq-local end-of-defun-function 'js-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)

  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))
  
  ;;  (setq c-current-comment-prefix  c-comment-prefix-regexp)
  
  ;;  (setq-local parse-sexp-ignore-comments t)
  ;;  (setq-local parse-sexp-lookup-properties t)
  
  ;; don't trust properties
  (setq parse-sexp-lookup-properties nil)
  
  ;; set up syntax hightlighting
  (setup-pjs-syntax-highlighting)

  ;; custom keybindings from menu
  (when (eq *emacs-environment-mode* :dev)
    (define-key *pjs-mode-map* "\C-c." '%pjs-find-definition)
    (define-key *pjs-mode-map* "\C-c," 'fi:lisp-find-next-definition)
    (define-key *pjs-mode-map* "\C-cc" '%pjs-list-who-calls))
  (define-key *pjs-mode-map* "\C-ce" 'compile-pjs-file)
  ;;  (define-key *pjs-mode-map* "\C-ck" 'check-ojs-region)
  ;;  (define-key *pjs-mode-map* "\C-cr" 'compile-pjs-region)
  (define-key *pjs-mode-map* "\C-c\C-b" 'save-and-compile-pjs-file)
  (define-key *pjs-mode-map* "\C-cs" 'save-and-compile-pjs-file)
  (define-key *pjs-mode-map* "\C-ct" 'trace-pjs-function)
  (define-key *pjs-mode-map* "\C-cR" 'pjs-reset-cache-on-reset)
  (define-key *pjs-mode-map* "\C-ch" 'open-ojs-documentation)

  ;; comment / un-comment
  (define-key *pjs-mode-map* "\C-c;" 'comment-region)
  (define-key *pjs-mode-map* "\C-c:" 'uncomment-region)

  ;; autoindentation on new line and add a closing } if needed
  (define-key *pjs-mode-map* (kbd "RET") 'newline-and-indent)
  ;;  (define-key *ojs-mode-map* (kbd "RET") 'ojs-mode-insert-lcurly-on-ret)
  ;; auto insert closing }
  ;;  (define-key *ojs-mode-map* (kbd "{") 'ojs-mode-insert-lcurly)
  
  ;; menu
  (easy-menu-define pjs-menu *pjs-mode-map* "Planisware Script Menu"
    (if (eq *emacs-environment-mode* :dev)
	'("Planisware Script"
	  ["Compile and load file..." compile-pjs-file
	   t]
	  ["Find function definition..." %pjs-find-definition
	   t]
	  ["Trace/Untrace function..." trace-pjs-function
	   t]
	  ["Planisware javascript documentation"  open-ojs-documentation
	   t]
	  ["Reset syntax caches" pjs-reset-cache-on-reset
	   t]
	  ["Planisware Script evaluator" switch-to-script-evaluator
	   t]		  
	  )
      '("Planisware Script"
	["Compile and load file..." compile-pjs-file
	 t]
	["Trace/Untrace function..." trace-pjs-function
	 t]
	["Planisware javascript documentation"  open-ojs-documentation
	 t]
	["Reset syntax caches" pjs-reset-cache-on-reset
	 t]
	["Planisware Script evaluator" switch-to-script-evaluator
	 t]
	)))

  ;; custom keymap
  (use-local-map *pjs-mode-map*)  
  
  ;; rebuild  function and vars cache on save and when we open a file
  ;;  (add-hook 'after-save-hook 'check-header nil t)
  ;;  (add-hook 'after-save-hook 'pjs-check-footer nil t)
  (add-hook 'after-save-hook 'pjs-reset-cache-on-save nil t)
  
  (add-hook 'find-file-hook 'pjs-reset-cache-on-save nil t)
;;  (add-hook 'find-file-hook 'pjs-reset-cache-on-compile nil t)

  ;; activete some semantic modes
  (global-semantic-mru-bookmark-mode 1)

  (setq semantic-idle-scheduler-idle-time 2))

;; autocomplete
(require 'ac-pjs)

;; pjs files are pjs modes
(setq auto-mode-alist (cons '("\\.pjs" . pjs-mode) auto-mode-alist))
