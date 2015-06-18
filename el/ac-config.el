(require 'auto-complete-config)

(ac-config-default)
(setq ac-auto-start nil)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.1)

;; always complete on shift tab
(define-key ac-mode-map (read-kbd-macro "<backtab>") (lambda () (interactive) (ac-trigger-key-command t)))
;;always complete on ctrl space
;;(define-key ac-mode-map (read-kbd-macro "C-SPC") (lambda () (interactive) (ac-trigger-key-command t)))

;; lisp modes
(add-to-list 'ac-modes 'fi:common-lisp-mode)
(add-to-list 'ac-modes 'fi:inferior-common-lisp-mode)
(add-to-list 'ac-modes 'fi:lisp-listener-mode)

(when *use-opx2-js-mode*
  ;; custom dictionaries
  (add-to-list 'ac-dictionary-directories (fullpath-relative-to-current-file "../packages/ac-dict"))

  ;; js modes
  (add-to-list 'ac-modes 'opx2-js-mode)
  (add-to-list 'ac-modes 'js-evaluator-mode))
