;; Load needed packages from MELPA automagically

;; MELPA source
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar *plw-required-packages* '(magit
				  auto-complete
				  ;;newcomment
				  fuzzy
				  tabbar
				  sr-speedbar))

(defun plw-packages-installed-p ()
  (loop for p in *plw-required-packages*
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (plw-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" "Done.")
  ;; install the missing packages
  (dolist (p *plw-required-packages*)
    (when (not (package-installed-p p))
      (message "Installing package %s ..." p)
      (package-install p)
      (message "Done."))))
