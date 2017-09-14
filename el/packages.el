;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT

;; Load needed packages from MELPA automagically

;; MELPA source
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar *plw-required-packages* '((magit 24 4)
				  auto-complete
				  ;;newcomment
				  fuzzy
				  tabbar
				  sr-speedbar
				  tangotango-theme
				  yasnippet
				  ))

(defun plw-packages-installed-p ()
  (loop for p in *plw-required-packages*
	when (not (package-installed-p (if (consp p) (car p) p))) do (return nil)
	finally (return t)))

(defun plw-install-package (p)
  (cond ((symbolp p)
	 (unless (package-installed-p p) (package-install p)))
	((consp p)
	 (cond ((or (> emacs-major-version (second p))
		    (and (= emacs-major-version (second p))
			 (>= emacs-minor-version (third p))))
		(unless (package-installed-p (car p)) (package-install (car p))))
	       (t
		(message (format "Ignoring installation of package %s because your version of Emacs (%s.%s) does not match its requirement which requires version %s.%s."
				 (car p)
				 emacs-major-version
				 emacs-minor-version
				 (second p)
				 (third p))))))))

(unless (plw-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" "Done.")
  ;; install the missing packages
  (dolist (p *plw-required-packages*)
    (when (not (package-installed-p (if (consp p) (car p) p)))
      (message "Installing package %s ..." p)
      (plw-install-package p)
      (message "Done."))))
