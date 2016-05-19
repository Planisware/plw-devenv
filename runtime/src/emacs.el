;; OPX2 working directory
(defvar *opx2-network-folder-work-path* "__OPX2_DIRECTORY__")

;; TODO : Packager les .el
(if (memq system-type '(cygwin32 windows-nt ms-windows ms-dos win386))
	(load "C:/acl90-smp.64/eli/fi-site-init.el")
  (load "/usr/local/acl90-smp.64/eli/fi-site-init.el"))

(load (format "%s/devenv/emacs-plw.el" *opx2-network-folder-work-path*))
(load (format "%s/opx2-runtime.el" *opx2-network-folder-work-path*))
