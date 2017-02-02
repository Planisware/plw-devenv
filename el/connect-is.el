;; -*- Coding: windows-1252 -*- 
;;  COPYRIGHT (C) PLANISWARE 2016
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************

(defvar *inside-connect-is* nil)

(defvar *ignore-commands* (list "ex" "exi" "exit" "ki" "kil" "kill"))

(defvar *ignored-commands-regexp* (concatenate 'string "\\s-*:" (regexp-opt *ignore-commands*) "\\s-*"))

(defvar *base-url-regexp* "^https?://.*:[0-9]+/")
(defvar *host-regexp* "^https?://\\([^/:]+\\)\\(?::[0-9]+\\)?/")

(defun start-connect-is (desc)
  (setq *inside-connect-is* t)
  (with-current-buffer "*common-lisp*"
    (when (and (fi::ensure-lep-connection)
	       (fi:eval-in-lisp "(cl:if (cl:fboundp 'http-utils::declare-additional-log-stream) t nil)"))
      (erase-buffer)
      (process-send-string (get-buffer-process "*common-lisp*")
			   "(http-utils::declare-additional-log-stream *standard-output*)")
      (fi:eval-in-lisp "(setq opx2-user::*db-in-prompt* t)")
      (text-mode)))
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (switch-to-listener))

(defun connect-is-with-url (url)
  (interactive "sUrl of the Intranet server: ")
  (let* ((file (make-temp-file "isconnect"))
	 (baseurl (progn (string-match *base-url-regexp* url)
			 (match-string 0 url)))
	 (host (progn (string-match *host-regexp* url)
		      (match-string 1 url))))
    (cond (baseurl
	   (let ((data (with-current-buffer (url-retrieve-synchronously (format "%scomint" baseurl))
			 (goto-char (point-min))
			 (re-search-forward "\n\n")
			 (prog1
			     (buffer-substring-no-properties (point) (line-end-position))
			   (kill-buffer)))))
	     (cond ((string= (substring data 0 1) "")
		    (with-temp-buffer
		      (insert data)
		      (write-region (point-min) (point-max) file))
		    (setq *script-compilation-mode* :remote)
		    (fi:start-interface-via-file host "*common-lisp*" file)
		    (start-connect-is url))
		   (t
		    (message "There was an error connecting to the Intranet server")))))
	  (t
	   (message "Invalid url")))))

(defun connect-is (host port)
  (interactive "sHost: \nnPort: ")
  (telnet host port)
  (let* ((buf (current-buffer))
	 (state :start)
	 (proc (get-buffer-process buf)))
    (when proc
      (telnet-simple-send proc "TELNET"))
    ;;search for CL-USER(2) before sending data
    (while (not (eq state :done))
      (cond ((and (search-backward "CL-USER(2):" (line-beginning-position) t)
		  (eq state :start))
	     (setq state :end)
	     (telnet-simple-send proc "(ignore-errors (let ((file (system:make-temp-file-name))) (when (excl:new-start-emacs-lisp-interface :port nil :announce-to-file file) (loop (when (probe-file file) (return))) (let ((in (open file :if-does-not-exist nil))) (when in (loop for line = (read-line in nil) while line do (format t  \"~a~%\" line)) (close in))) (delete-file file))))
"))
	    ((search-backward "CL-USER(3):" (line-beginning-position) t)
	     (telnet-simple-send proc "(:exit-telnet)")
	     (setq state :done))
	    (t
	     (accept-process-output proc 0.5))))
    ;;Answer we look for is after CL-USER(2)
    (when (search-backward "CL-USER(2):" nil t)      
      (forward-line 1)
      (while (and (= (line-beginning-position) (line-end-position))
		  (= (forward-line 1) 0)))
      (let ((data (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	    (file (make-temp-file "isconnect")))
	(with-temp-buffer 
	  ;;write data to a temp file and start emacs lisp interface
	  (insert data)
	  (write-region (point-min) (point-max) file)
	  (setq *script-compilation-mode* :remote)
	  (fi:start-interface-via-file host "*common-lisp*" file))
	(start-connect-is (format "server %s port %s" host port))
	(delete-file file)	
	(kill-buffer buf)))))

(defun fi:subprocess-send-input ()
  "Send input to the subprocess.  At end of buffer, send all text after
last output as input to the subshell, including a newline inserted at the
end.  When not at end, copy current line to the end of the buffer and
send it,after first attempting to discard any prompt at the beginning of
the line by matching the regexp that is the value of
the buffer-local fi::prompt-pattern, which is initialized by each
subprocess mode."
  (interactive)
  (if fi::shell-completions-window (fi::shell-completion-cleanup))
  (end-of-line)
  (if (eobp)
      (progn
	(setq fi::last-input-start
	  (marker-position
	   (process-mark (get-buffer-process (current-buffer)))))
	(if (and (on-ms-windows) (not *on-windows-nt*))
	    (insert "\n\r")
	  (insert "\n"))
	(setq fi::last-input-end
	  (if (and (on-ms-windows) (not *on-windows-nt*))
	      (1- (point))
	    (point))))
    (let ((max (point)))
      (beginning-of-line)
      (re-search-forward fi::prompt-pattern max t))
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (setq fi::last-input-start (point))
      (insert copy)
      (setq fi::last-input-end (point))))
  (fi::subprocess-watch-for-special-commands)
  (let ((process (get-buffer-process (current-buffer))))
    (cond ((null *inside-connect-is*)
	   (process-send-region process fi::last-input-start fi::last-input-end))
	  ((save-excursion
	     (goto-char fi::last-input-start)
	     (looking-at *ignored-commands-regexp*))
	   (process-send-string process "
"))
	  (t
	   (process-send-region process fi::last-input-start fi::last-input-end)))
      (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
      (when (and (on-ms-windows) (not *on-windows-nt*))
	(delete-char -1))
      (set-marker (process-mark process) (point))))

