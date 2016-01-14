;;; semantic configuration for pjs files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'semantic)
(require 'semantic/bovine)

(load "~/Documents/opx2/devenv/grammars/wisent-javascript-jv-wy.el")

(require 'wisent-javascript-jv-wy)


;;;###autoload
(defun semantic-default-js-setup ()
  "Setup hook function for pjs files and Semantic."
  (wisent-javascript-jv-wy--install-parser)

  (setq
   ;; Lexical analysis
   ;;  semantic-lex-number-expression semantic-java-number-regexp
   semantic-lex-analyzer 'javascript-lexer-jv
   wisent-parse-verbose-flag t
   ) 
  )

;;(add-hook 'pjs-mode-hook 'semantic-default-pjs-setup)
