;;;;
;;;; COPYRIGHT (C) PLANISWARE $Date$ 
;;;;
;;;; All Rights Reserved
;;;;
;;;; This program and the information contained herein are confidential to
;;;; and the property of PLANISWARE and are made available only to PLANISWARE
;;;; employees for the sole purpose of conducting PLANISWARE business.
;;;;
;;;; This program and copy therof and the information contained herein shall
;;;; be maintained in strictest confidence ; shall not be copied in whole or
;;;; in part except as authorized by the employee's manager ; and shall not
;;;; be disclosed or distributed (a) to persons who are not PLANISWARE employees,
;;;; or (b) to PLANISWARE employees for whom such information is not necessary in
;;;; connection with their assigned responsabilities.
;;;;
;;;; There shall be no exceptions to the terms and conditions set forth
;;;; herein except as authorized in writing by the responsible PLANISWARE General
;;;; Manager.

;;;;
;;;; FILE    : $RCSfile$
;;;;
;;;; AUTHOR  : $Author$
;;;;
;;;; VERSION : $Id$
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile$" :id "$Id$" :version "$Revision$" :date "$Date$ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (when (fboundp :require-patch) (:require-patch ""))
;;;; HISTORY :
;;;; $Log$
;;;; Revision 3.5  2011/07/23 13:30:16  folli
;;;; Debug emacs windows
;;;;
;;;; Revision 3.4  2011/07/22 14:05:57  folli
;;;; Window compat 2
;;;;
;;;; Revision 3.3  2011/07/22 14:05:37  folli
;;;; Window compat
;;;;
;;;; Revision 3.2  2011/07/22 13:56:47  folli
;;;; Xemacs compat
;;;;
;;;; Revision 3.1  2011/07/22 13:43:21  folli
;;;; Colorize background to know if a function has been redefined
;;;; or not
;;;;  (header added automatically)
;;;;
;; -*-no-byte-compile: t; -*-

;;Force no byte compilation (elc compiled by xemacs won't be readable via emacs and
;; the other way around)

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



	 
