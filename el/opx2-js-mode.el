;; -*- coding: windows-1252 -*- 
;;  COPYRIGHT (C) PLANISWARE 2016
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************

(require 'cc-mode)

(defvar opx2-js-mode-syntax-table
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
    table)
  "Syntax table used in JavaScript mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto add closing }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ojs-mode-insert-lcurly-on-ret ()
  (interactive)
  ;; do we have a { at the point ?
  (if (fast-looking-back "{")
      (let ((pps (syntax-ppss)))
	(when (and (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
	  (c-indent-line-or-region)
	  (insert "\n\n}")
	  (c-indent-line-or-region)
	  (forward-line -1)
	  (c-indent-line-or-region)))
    (newline-and-indent)))

(defun ojs-mode-insert-lcurly ()
  (interactive)
  (insert "{")
  (let ((pps (syntax-ppss)))
    (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
      (c-indent-line-or-region)
      (insert "\n\n}")
      (c-indent-line-or-region)
      (forward-line -1)
      (c-indent-line-or-region))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu and keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ojs-mode-map* (make-sparse-keymap))

;; we don't want to stack definition declarations
(setq fi:maintain-definition-stack nil)

;; try to get a full symbol from the position given
(defun find-symbol-at-point (start end)
  ;; is the previous char a : ?
  (save-excursion
    (goto-char start)
    (backward-char)
    (when (looking-at ":")
      (while (and (not (or (looking-at "[ (]")
			   (looking-at "^")))
		  (> (point) (point-min)))
	(backward-char))
      (if (looking-at "^")
	  (setq start (point))
	(setq start (1+ (point))))))
  (buffer-substring-no-properties start end))

;; <ctrl-c .> in ojs file
;; works with ojs and lisp file and properly do the search
(defun %ojs-find-definition (tag)
  (interactive
   (if current-prefix-arg
       '(t)
     (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
  (if (string-match ":" tag)
      (fi::lisp-find-definition-common tag nil)
    (fi::lisp-find-definition-common (concat "js::" tag) nil)))

(defun %ojs-list-who-calls (tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (fi::get-default-symbol "Lisp locate source" t t)))))
  (if (string-match ":" tag)
      (fi:list-who-calls tag)
    (fi:list-who-calls (concat "js::" tag))))

(defvar *ojs-compilation-buffer-name* "*OJS compilation traces*")

(defun compile-ojs-file ()
  (interactive)
  (do-compile-and-sync-ojs-file :compile)
  )

(defun save-and-compile-ojs-file ()
  (interactive)
  (save-buffer)
  (compile-ojs-file)
  )

(defun open-lisp-script-file ()
  (interactive)
  (when (fi::ensure-lep-connection)
    (let* ((script-name (or (get-script-name)
			    (file-name-base (buffer-file-name))))
	   (lisp-file (fi:eval-in-lisp (format "(ignore-errors (namestring (jvs::script-lisp-file nil \"%s\")))" script-name))))
      (when lisp-file
	(find-file lisp-file)))))

(defun check-ojs-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let* ((selection (buffer-substring-no-properties beg end))
	 (buffer-name *ojs-compilation-buffer-name*)
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer)))
    (unless proc
      (setq proc
	    (fi:open-lisp-listener
	     -1
	     *ojs-compilation-buffer-name*))
      (set-process-query-on-exit-flag (get-process buffer-name) nil))
    (set-process-filter proc 'ojs-compilation-filter)
    (switch-to-buffer-other-window buffer-name t)
    (erase-buffer)
    (process-send-string *ojs-compilation-buffer-name* (format "(javascript::check-js-syntax %S)\n" selection))))

(defun compile-ojs-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let* ((selection (buffer-substring-no-properties beg end))
	 (buffer-name *ojs-compilation-buffer-name*)
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer))
	 (v2 (if (eq major-mode 'pjs-mode) "cl:t" "nil"))
	 )
    (unless proc
      (setq proc
	    (fi:open-lisp-listener
	     -1
	     *ojs-compilation-buffer-name*))
      (set-process-query-on-exit-flag (get-process buffer-name) nil))
    (set-process-filter proc 'ojs-compilation-filter)
    (switch-to-buffer-other-window buffer-name t)
    (erase-buffer)
    (process-send-string *ojs-compilation-buffer-name* (format "(jvs::emacs-check-and-compile-script %S %s)\n" selection v2))))


(defvar *compiled-script-window* nil)

(defun get-script-name ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^//\\s-*PLWSCRIPT\\s-*:\\s-*\\(.*\\)\\s-*$" (point-max) t)
      (match-string-no-properties 1))))

(defun do-compile-and-sync-ojs-file (type)
  ;; find the script name
  (when (ojs-configuration-ok)
    (let* ((script-name      (file-name-base (buffer-file-name)))
	   (script           (or ;; try to get a comment //PLWSCRIPT :
			      (save-excursion
				(goto-char (point-min))
				(if (re-search-forward "^/+\\s-*PLWSCRIPT\\s-*:\\s-*\\([[:alnum:]_-]*\\)\\(\\s-+|\\s-*\\([[:alnum:], ]*\\)\\)?" (point-max) t)
				    (match-string-no-properties 1)
				  nil))
			      (fi:eval-in-lisp (format "(jvs::find-script \"%s\")" script-name))))
	   (options (when (match-string-no-properties 3)
		      (split-string (match-string-no-properties 3) "[ \f\t\n\r\v,]+")))
	   (buffer-name *ojs-compilation-buffer-name*)
	   (buffer (or (get-buffer buffer-name)
		       (get-buffer-create buffer-name)))
	   (proc (get-buffer-process buffer))
	   (js-mode major-mode)
	   (filename (buffer-file-name))
	   )
;;      (message (format "options are %s" options))
      (setq *compiled-script-window* (selected-window))
      (if (and script (fi:eval-in-lisp (format "(if (object::get-object 'jvs::javascript %S) t nil)" script)))
	  (catch 'exit
	    ;; checks that the file matches only for synchronize	    
	    (save-buffer)
	    (switch-to-buffer-other-window buffer-name t)
	    ;; we erase previous content
	    (erase-buffer)
	    ;; run a new listener if needed
	    (unless proc
	      (setq proc
		    (fi:open-lisp-listener
		     -1
		     *ojs-compilation-buffer-name*))
	      (set-process-query-on-exit-flag (get-process buffer-name) nil))
	    (set-process-filter proc 'ojs-compilation-filter)
	    ;; reset vars as needed
	    (js-reset-vars (if (eq js-mode 'pjs-mode) 'pjs-compile 'ojs-compile))
	    (cond ((eq type :compile)
		   (if (member "PROPAGATE" options)
		       (process-send-string *ojs-compilation-buffer-name* (format "(cl:if (cl:fboundp :recompile-one-js) (:recompile-one-js \"%s\" :source \"%s\" :propagate cl:t) (:rjs-one \"%s\"))\n" script filename script))
		     (process-send-string *ojs-compilation-buffer-name* (format "(cl:if (cl:fboundp :recompile-one-js) (:recompile-one-js \"%s\" :source \"%s\") (:rjs-one \"%s\"))\n" script filename script)))
		   ))
	(message "Script %s not found" script-name)))))

(defun generate-search-string (strings)
  (cond ((= (length strings) 1) (format "function\\s-+%s\\s-*([[:word:]_, ]*)" (downcase (car strings))))
	((and (>= (length strings) 4)
	      (string= (car strings) "method")
	      (string= (third strings) "on"))
	 (let* ((dot (position ?. (fourth strings)))
		(class-name (cond (dot
				   (format "\\(?:%s\\)?\\.%s" (substring-no-properties (fourth strings) 0 dot)
					   (substring-no-properties (fourth strings) (1+ dot))))
				  (t
				   (fourth strings)))))
	   (format "method\\s-+%s\\s-+on\\s-+%s\\s-*(.*" (downcase (second strings)) class-name)))))

(defun ojs-compilation-filter (proc string)
  (let (error?)
    (cond ((and (stringp string)
		(string-match "Error while compiling the script" string)
		(string-match "At line:\\([0-9]+\\),character:\\([0-9]+\\)" string))
	   ;; we move the point on the original buffer to the error
	   (let ((line (string-to-number (match-string 1 string)))
		 (point (string-to-number (match-string 2 string)))
		 (buf (current-buffer)))
	     (with-current-buffer (window-buffer *compiled-script-window*)
	       (when (> line 0)
		 (goto-line line)
		 (beginning-of-line)
		 (when (> point 0)		   
		   (forward-char point))
		 (set-window-point *compiled-script-window* (point)))))
	   (fi::subprocess-filter proc string))
	  ((and (stringp string)
		(string-match "Error while compiling the script" string)
		(string-match "In \\(\\(.\\|\n\\)*?\\):" string))
	   (let* ((error-context (match-string-no-properties 1 string))
		  (strings (split-string error-context))
		  (search-string (generate-search-string strings))
		  )
	     (when (window-buffer *compiled-script-window*)
	       (with-current-buffer (window-buffer *compiled-script-window*)
		 (when search-string
		   (goto-char (point-min))		 
		   (when (re-real-search-forward search-string nil t)
		     (set-window-point *compiled-script-window* (point)))))))
	   (fi::subprocess-filter proc string))
	  ((and (stringp string)
		(string-match "\\`[[:upper:]-]+([0-9]+): \\'" string)) ;; exit when we go back to the top level (ie :res, :pop, etc)
	   ;;(delete-process proc)
	   )
	  ((and (stringp string)
		(string-match ":EXIT-JS" string)) ;; exit when we read this, returned by the compilation functions
	   (with-current-buffer (get-buffer *ojs-compilation-buffer-name*)
	     (insert "----------------------------------------------------------------------------------------------"))
	   (fi::subprocess-filter proc (substring string 0 (string-match ":EXIT-JS" string)))	   
	   ;;(delete-process proc)
	   )	  
	  (t
	   (fi::subprocess-filter proc string)))))

(defun trace-ojs-function(tag)
  (interactive
   (if current-prefix-arg
       '(nil)
     (list (car (fi::get-default-symbol "Lisp (un)trace function" t t)))))
  (let ((js-symbol (concat "js::" tag)))
    (fi:toggle-trace-definition js-symbol)))

;; Javascript documentation

;;;
(defun open-ojs-documentation ()
  (interactive)
  (when (fi::lep-open-connection-p)
    (let ((filename (fi:eval-in-lisp "(main::call-script-doc)")))
      (when filename
	(browse-url-of-file filename)))))

(defun ojs-reset-cache-on-reset ()
  (interactive)
  (js-reset-vars 'ojs-reset)
  (when (buffer-file-name)
    (save-buffer)
    (plw-refresh-file-buffer (buffer-file-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-derived-mode opx2-js-mode prog-mode "OPX2 javascript"
(define-derived-mode opx2-js-mode prog-mode "OPX2 javascript"
  :syntax-table opx2-js-mode-syntax-table

  ;; load a little bit of cc-mode for indentation
  (c-initialize-cc-mode t)
  (c-init-language-vars-for 'c-mode)
  (c-common-init 'c-mode)

  ;; set up syntax hightlighting
  (setup-ojs-syntax-highlighting)

  ;; custom keybindings from menu
  (define-key *ojs-mode-map* "\C-c." '%ojs-find-definition)
  (define-key *ojs-mode-map* "\C-c," 'fi:lisp-find-next-definition)
  (define-key *ojs-mode-map* "\C-cc" '%ojs-list-who-calls)
  (define-key *ojs-mode-map* "\C-ce" 'compile-ojs-file)
  (define-key *ojs-mode-map* "\C-ck" 'check-ojs-region)
  (define-key *ojs-mode-map* "\C-cr" 'compile-ojs-region)
  (define-key *ojs-mode-map* "\C-c\C-b" 'save-and-compile-ojs-file)
;;  (define-key *ojs-mode-map* "\C-cs" 'save-compile-and-sync-ojs-file)
  (define-key *ojs-mode-map* "\C-ct" 'trace-ojs-function)
  (define-key *ojs-mode-map* "\C-ch" 'open-ojs-documentation)
  (define-key *ojs-mode-map* "\C-cR" 'ojs-reset-cache-on-reset)

;;  (define-key *ojs-mode-map* "\C-cl" 'lock-file)
;;  (define-key *ojs-mode-map* "\C-cu" 'unlock-file)

  ;; comment / un-comment
  (define-key *ojs-mode-map* "\C-c;" 'comment-region)
  (define-key *ojs-mode-map* "\C-c:" 'uncomment-region)

  ;; autoindentation on new line and add a closing } if needed
  (define-key *ojs-mode-map* (kbd "RET") 'newline-and-indent)
  ;;  (define-key *ojs-mode-map* (kbd "RET") 'ojs-mode-insert-lcurly-on-ret)
  ;; auto insert closing }
  ;;  (define-key *ojs-mode-map* (kbd "{") 'ojs-mode-insert-lcurly)
  
  ;; menu
  (easy-menu-define ojs-menu *ojs-mode-map* "OPX2 Javascript Menu"
    '("OPX2 Javascript"
      ["Compile and load file..." compile-ojs-file
       t]
      ["Check syntax of selected region" check-ojs-region
       t]	    
      ;; ["Compile, load and synchronize file..." save-compile-and-sync-ojs-file
      ;;  t]
      ["Compile and run selected region" compile-ojs-region
       t]
      ["Find function definition..." %ojs-find-definition
       t]
      ["Trace/Untrace function..." trace-ojs-function
       t]
      ["OPX2 javascript documentation"  'open-ojs-documentation
       t]
      ["Reset syntax caches" ojs-reset-cache-on-reset
       t]
      ))

  ;; custom keymap
  (use-local-map *ojs-mode-map*)  
  
  ;; rebuild  function and vars cache on save and when we open a file
  (add-hook 'after-save-hook 'ojs-reset-cache-on-save nil t)
  (add-hook 'find-file-hook 'ojs-reset-cache-on-save nil t)
  )

;; kludge : in opx2 script, the first line sets the mode to C++, and we want to avoid that
;; so we call our function from the c++ mode hook
(defun override-c++-mode ()
  (cond ((equal (downcase (substring buffer-file-name -3 nil)) "ojs")
	 (opx2-js-mode))
	((equal (downcase (substring buffer-file-name -3 nil)) "pjs")
	 (pjs-mode))))


(when *use-opx2-js-mode*
  (add-hook 'c++-mode-hook 'override-c++-mode))

;; replace for new files
(defun put-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]"
  (let ((pair (assoc item alist)))
    (if pair
	(progn
	  (setcdr pair value)
	  alist)
      (cons (cons item value) alist)
      )))
