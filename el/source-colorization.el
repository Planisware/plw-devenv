;; -*-no-byte-compile: t; -*-

;;Force no byte compilation (elc compiled by xemacs won't be readable via emacs and
;; the other way around)


(defvar plw-source-color-ok "honeydew")
(defvar plw-source-color-not-ok "MistyRose1")

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
    (let ((colors (fi:eval-in-lisp (format "(:emacs-source-file \"%s\")" (buffer-file-name (window-buffer win))))))
      (when colors
	(dolist (color-spec colors)
	  (let* ((id (first color-spec))
		 (start (second color-spec))
		 (end (third color-spec))
		 (status (fourth color-spec))
		 (o (dolist (o (overlays-in start end) nil)
		      (when (equal-including-properties
			     (overlay-get o 'plw-colorize-source) id)
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
	    
	    (overlay-put o 'face `(:background ,(if status plw-source-color-ok plw-source-color-not-ok)))
	    (overlay-put o 'plw-colorize-source id)))))))
      
;;(define-minor-mode plw-source-color-mode
;;  "Toggle display of source indication in the left margin area"
;; (cond (plw-source-color-mode
;;	 (add-hook 'after-change-functions 'plw-source-code-after-change))
;;	(t
;;	 (remove-hook 'after-change-functions 'plw-source-code-after-change t))))



	 