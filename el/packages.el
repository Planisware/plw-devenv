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
				  sr-speedbar))

(defun plw-packages-installed-p ()
  (loop for p in *plw-required-packages*
	when (not (package-installed-p (if (consp p) (car p) p))) do (return nil)
	finally (return t)))

(defun plw-install-package (p)
  (cond ((symbolp p)
	 (unless (package-installed-p p) (package-install p)))
	((consp p)
	 (cond ((and (>= emacs-major-version (second p))
		     (>= emacs-minor-version (third p)))
		(unless (package-installed-p (car p)) (package-install (car p))))
	       ((y-or-n-p (format "You version of Emacs (%s.%s) does not match the requirement for the package %s, which requires version %s.%s. Do you want to continue without the package %s ?"
				  emacs-major-version
				       emacs-minor-version
				       (car p)
				       (second p)
				       (third p)
				       (car p)))
		;; do nothing here
		)
	       (t
		(error "Uncompatible version of Emacs (%s.%s), wanted at least %s.%s to use package %s"
		       emacs-major-version emacs-minor-version
		       (second p) (third p) (car p)))))))
  
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
