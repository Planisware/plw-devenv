;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT

(dolist (file (list "%seli/fi-site-init.el"
                    "%sdevenv/emacs-plw-ext.el"
                    "%sopx2-runtime.el"
                    "%sdevenv/emacs4dummies.el"
                    ))
  (let ((path (format file *opx2-network-folder-work-path*)))
    (when (file-exists-p path)
      (load path))))
