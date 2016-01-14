(require 'auto-complete)

;;;;; configuration of the hook

;; definition of a new autocomplete mode
(defun pjs-setup-auto-complete-mode ()
  "Setup ac-js2 to be used with auto-complete-mode."
;;  (message "Setting autocomplete")
  ;; js functions defined in the file
  (setq ac-sources nil)
  ;;(setq ac-sources '(ac-source-ojs-functions ac-source-ojs-methods ac-source-ojs-vars ac-source-ojs-kernel))
  
  (add-to-list 'ac-sources 'ac-source-ojs-functions)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-vars)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-kernel)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-dictionary)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-members)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-classes)

  ;; initialize kernel completion
;;  (ojs-kernel-ac-init)

  ;; classes completion
;;  (ojs-classes-ac-init)

  ;; dictionary cache
;;  (setq *ac-dictionary-candidates-cache* (ac-file-dictionary (concat *opx2-network-folder-work-path* "devenv/el/dict/opx2-js-mode")))
  
  ;; display doc quickly
  (setq ac-quick-help-delay 0.1)
  ;; autocomplete only when I ask for it
  (setq ac-auto-start nil)
)

(add-hook 'pjs-mode-hook 'pjs-setup-auto-complete-mode)
;;(add-hook 'js-evaluator-mode-hook 'opx2-js-setup-auto-complete-mode)
