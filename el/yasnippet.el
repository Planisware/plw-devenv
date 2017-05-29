;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT

;;; yasnippet configuration
(require 'yasnippet)

(yas-global-mode 1)

;; custom snippets
(push (format "%s/devenv/el/yasnippet" *opx2-network-folder-work-path*) yas-snippet-dirs)



