;; -*- coding: windows-1252 -*- 
;;  COPYRIGHT (C) PLANISWARE 2017
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************

;;; yasnippet configuration
(require 'yasnippet)

(yas-global-mode 1)

;; custom snippets
(push (format "%s/devenv/el/yasnippet" *opx2-network-folder-work-path*) yas-snippet-dirs)



