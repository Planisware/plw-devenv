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

(when (eq *javascript-evaluator-mode* :repl)
  (global-set-key [f3] 'switch-to-script-evaluator))

;; mode pour la coloration syntaxique

(setq *js-keywords*
      '(("^Warning[ ]*:.*" . font-lock-warning-face)
	("^Error[ ]*:.*" . font-lock-variable-name-face)
	;;   ("^Return  : .*" . font-lock-type-face)
	("^Return[ ]*:[^\"\n]*" . font-lock-type-face)
	;;   ("^Applet used[ ]*: .*" . font-lock-function-name-face)
	("^Info[ ]*:.*" . font-lock-function-name-face)
	;; ("JS: " . font-lock-string-face)
	)
      )

(defvar *js-evaluator-map* (make-sparse-keymap))

(defvar js-evaluator-mode-syntax-table
  (let ((table (make-syntax-table fi:lisp-mode-syntax-table)))
    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)
    ;;    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table used in JavaScript mode.")

;;(define-derived-mode js-evaluator-mode lisp-listener-mode
(define-derived-mode js-evaluator-mode prog-mode
  :syntax-table js-evaluator-mode-syntax-table
  ;; js font lock
  (setq font-lock-defaults '(*js-keywords*))

  (setq mode-name "Javascript evaluator")
  ;; js comments
  (set (make-local-variable 'comment-start) "// ")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//[ \t]*")

  (set (make-local-variable 'comment-end) "")

  ;;  (fi::initialize-mode-map 'fi:lisp-listener-mode-map
  ;;			   'fi:lisp-listener-mode-super-key-map
  ;;			   'tcp-lisp)
  ;; (use-local-map fi:lisp-listener-mode-map)
  (define-key *js-evaluator-map* (kbd "\C-c.") '%ojs-find-definition)
  (define-key *js-evaluator-map* "\C-c," 'fi:lisp-find-next-definition)
  (define-key *js-evaluator-map* "\C-cc" '%ojs-list-who-calls)
  (define-key *js-evaluator-map* "\C-ct" 'trace-ojs-function)
  (define-key *js-evaluator-map* "\C-c\C-c" 'fi:interrupt-listener)
  
  (define-key *js-evaluator-map* (kbd "RET") 'javascript-evaluator-newline)
  ;;  (define-key *js-evaluator-map* (kbd "RET") 'javascript-evaluator-return)

  (use-local-map *js-evaluator-map*)  
  )

(defun fi:open-lisp-listener (&optional buffer-number buffer-name
					setup-function command mode)
  "Open a connection to an existing Common Lisp process, started with the
function fi:common-lisp, and create a Lisp Listener (a top-level
interaction).  The Common Lisp can be either local or remote.  The name of
the buffer is \"*lisp-listener*\" with an optional suffix of \"<N>\", for
prefix arguments > 1.  If a negative prefix argument is given, then the
first \"free\" buffer name is found and used.  When called from a program,
the buffer name is the second optional argument."
  (interactive "p")
  (if fi::started-via-file
      (fi::ensure-lep-connection)
    (if (or (null fi::common-lisp-backdoor-main-process-name)
	    (not (fi:process-running-p
		  (get-process fi::common-lisp-backdoor-main-process-name)
		  buffer-name)))
	(error "Common Lisp must be running to open a lisp listener.")))
  (if fi::started-via-file
      (fi::make-tcp-connection (or buffer-name "lisp-listener")
			       buffer-number
			       'fi:lisp-listener-mode
			       fi:common-lisp-prompt-pattern
			       fi::lisp-host
			       fi::lisp-port
			       fi::lisp-password
			       (or setup-function 'fi::setup-tcp-connection))
    (let* ((buffer (process-buffer
		    (get-process fi::common-lisp-backdoor-main-process-name)))
	   (proc (fi::make-tcp-connection (or buffer-name "lisp-listener")
					  buffer-number
					  (if mode mode
					    'fi:lisp-listener-mode)
					  fi:common-lisp-prompt-pattern
					  (fi::get-buffer-host buffer)
					  (fi::get-buffer-port buffer)
					  (fi::get-buffer-password buffer)
					  (or setup-function
					      'fi::setup-tcp-connection))))
      (when command
	(process-send-string proc command))
      proc)))

;;; when we go back to the top level, relaunch the repl
(defun javascript-evaluator-filter (proc string)
  (let ((case-fold-search nil))
    (cond ((and (stringp string)
		(string-match "\\`[[:upper:]-]+([0-9]+): \\'" string));; exit when we go back to the top level (ie :res, :pop, etc)
	   (process-send-string proc "(jvs::js-repl)")
	   )
	  ;;	  ((and (stringp string)
	  ;;		(string-match ":EXIT-JS" string)) ;; exit when we read this, returned by the compilation functions
	  ;;	   (fi::subprocess-filter proc (substring string 0 (string-match ":EXIT-JS" string)))
	  ;;	   (delete-process proc))
	  (t
	   (fi::subprocess-filter proc string)))))

(defvar *javascript-evaluator-initialize* nil)

(defun switch-to-script-evaluator ()
  (interactive)
  (let* ((buffer-name "*javascript-evaluator*")
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 (proc (get-buffer-process buffer))
	 )
    (cond ((ojs-configuration-ok)
	   (if (fi:process-running-p proc buffer-name)
	       (fi::switch-to-buffer-new-screen buffer-name)
	     (progn
	       (setq proc (fi:open-lisp-listener -1 buffer-name 'fi::setup-tcp-connection "(jvs::js-repl)" 'js-evaluator-mode))
	       (set-process-filter proc 'javascript-evaluator-filter)
	       (sleep-for 0.5)
	       (process-send-string proc "\n")
	       (when *javascript-evaluator-initialize*
		 (dolist (str *javascript-evaluator-initialize*)
		   (process-send-string proc (format "%s\n" str))))
	       )))
	  (t
	   (fi::switch-to-buffer-new-screen buffer-name)
	   (print "javascript repl not found" buffer)
	   ))))

(defvar *javascript-prompt-regexp* "^[OP]JS([0-9]+)\\(\\[[A-Za-z0-9]+\\]\\)?\\(\\[\\.+\\]\\)?: ")

(defun javascript-evaluator-newline ()
  (interactive) 
  (if (or (eobp) (eq (line-end-position) (point-max)))
      (fi:subprocess-send-input) ;; at the end of the buffer, we send the expression    
    (let ((start
	   (save-excursion
	     (move-beginning-of-line nil)
	     (and (re-search-forward *javascript-prompt-regexp* (line-end-position) t)
		  (point))))
	  (end (line-end-position)))
      (when (and start end)
	(fi:subprocess-input-region start end)))))
