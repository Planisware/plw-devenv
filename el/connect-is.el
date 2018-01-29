;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT


(defvar *connect-is-use-socks* nil)

(require 'socks)

;; example of socks proxy definition :
;;  if your putty connection contains a redirection from local port 8165 in Dynamic mode
;;(setq socks-server (list "Default server" "localhost"  8165 4))

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
      (setq frame-title-format (format "[Connected to %s]" desc))
      (text-mode)))
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (switch-to-listener))

(defun connect-is-with-url (url)
  (interactive "sUrl of the Intranet server: ")
  (let ((host (progn (string-match *host-regexp* url)
		     (match-string 1 url))))
    (%connect-is-with-url url host nil)))

(defun connect-is-with-socks (url port-number)
  (interactive "sUrl of the Intranet server: \nnPort: ")
  (%connect-is-with-url url "localhost" port-number))

(defun %connect-is-with-url (url host port)
  (if (and port (stringp port))
      (setq port (string port)))
  (let ((file (make-temp-file "isconnect"))
	(baseurl (progn (string-match *base-url-regexp* url)
			(match-string 0 url)))
	(*connect-is-use-socks* (if port t nil))
	(socks-server (list "Default server" "localhost"  port 4)))
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

(defun connect-is-old (host port)
  (interactive "sHost: \nnPort: ")
  (telnet host port)
  (let* ((buf (current-buffer))
	 (state :start)
	 (proc (get-buffer-process buf)))
    (when proc
      (telnet-simple-send proc "TELNET"))
    
    ))

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

(defun fi::open-network-stream (name buffer host service)
  (let ((tries fi::open-network-stream-retries))
    (block fi::open-network-stream
      (dotimes (i tries)
	(condition-case condition
	    (return-from fi::open-network-stream
	      ;; DLM 26-JAN-18 : use SOCKS if needede
	      (if *connect-is-use-socks* 
		  (socks-open-network-stream name buffer host service)
		(open-network-stream name buffer host service)))
	  (error
	   (cond
	    ((< i (1- tries))
	     (message "open-network-stream failed, retrying...")
	     (sleep-for 1))
	    (t
	     (setq fi::last-network-condition condition)
	     (when (not fi::muffle-open-network-stream-errors)
	       (cond
		((and (not (on-ms-windows))
		      (not (file-readable-p "/etc/hosts")))
		 (fi:error "
Can't connect to host %s.  This is probably due to /etc/hosts not being
readable.  The error from open-network-stream was:
  %s"
			   host (car (cdr condition))))
		(t
		 (cond
		  ((and (string-match "xemacs" emacs-version)
			(string= "21.4.17" (symbol-value 'emacs-program-version)))
		   (fi:error "
Can't connect to host %s.  The error from open-network-stream was:
  %s

You are running XEmacs 21.4.17, which has been known to contain a bug
that prevents open-network-stream to signal an error when a numerical
port is passed.  Try this workaround to the bug, by putting the following
form into your .emacs:

   (defadvice open-network-stream (around make-ports-be-strings)
     (when (numberp service)
       (setq service (format \"%%d\" service)))
      ad-do-it)"
			     host (car (cdr condition))))
		  (t
		   (fi:error "
Can't connect to host %s.  The error from open-network-stream was:
  %s"
			     host (car (cdr condition))))))))
	     nil))))))))

;;socks.el
(defun socks-open-network-stream (name buffer host service)
  (message (format "connect to %s %d" host service))
  (let* ((route (socks-find-route host service))
	 proc info version atype)
    (if (not route)
	(socks-original-open-network-stream name buffer host service)
      (setq proc (socks-open-connection route)
	    info (gethash proc socks-connections)
	    version (gethash 'server-protocol info))
      (cond
       ((equal version 4)
	;; DLM 26-JAN-18 : difficult to use nslookup on Windows,
	;;  this trick avoids it
	(setq host 
	      (if (equal host "localhost")
		  (list 127 0 0 1)
		(socks-nslookup-host host)))
	(if (not (listp host))
	    (error "Could not get IP address for: %s" host))
	(setq host (apply 'format "%c%c%c%c" host))
	(setq atype socks-address-type-v4))
       (t
	(setq atype socks-address-type-name)))
      (socks-send-command proc
			  socks-connect-command
			  atype
			  host
			  (if (stringp service)
			      (or
			       (socks-find-services-entry service)
			       (error "Unknown service: %s" service))
			    service))
      (puthash 'buffer buffer info)
      (puthash 'host host info)
      (puthash 'service host info)
      (set-process-filter proc nil)
      (set-process-buffer proc (if buffer (get-buffer-create buffer)))
      proc)))
