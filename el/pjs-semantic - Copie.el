;;; semantic configuration for pjs files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'semantic)
(require 'semantic/bovine)

(load "~/Documents/opx2/devenv/grammars/wisent-pjs-wy.el")

(require 'wisent-pjs-wy)

;;; Lexer
;;
(define-lex semantic-pjs-lexer
  "PJS lexer"
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  
  ;;;; Auto-generated analyzers.
  wisent-pjs-wy--<number>-regexp-analyzer
  wisent-pjs-wy--<string>-sexp-analyzer
  ;; Must detect keywords before other symbols
  wisent-pjs-wy--<keyword>-keyword-analyzer
  wisent-pjs-wy--<symbol>-regexp-analyzer
  wisent-pjs-wy--<punctuation>-string-analyzer
  wisent-pjs-wy--<block>-block-analyzer
  ;; In theory, Unicode chars should be turned into normal chars
  ;; and then combined into regular ascii keywords and text.  This
  ;; analyzer just keepqs these things from making the lexer go boom.
  ;;  wisent-java-tags-wy--<unicode>-regexp-analyzer
  semantic-lex-number
  semantic-lex-string
  semantic-lex-symbol-or-keyword
  ;;;;
  semantic-lex-default-action)

;;;###autoload
(defun semantic-default-pjs-setup ()
  "Setup hook function for pjs files and Semantic."
  (wisent-pjs-wy--install-parser)

 (setq
   ;; Lexical analysis
;;  semantic-lex-number-expression semantic-java-number-regexp
  semantic-lex-analyzer 'semantic-pjs-lexer
  ;; Parsing
;;  semantic-tag-expand-function 'semantic-java-expand-tag
   ;; Environment
;;   semantic-imenu-summary-function 'semantic-format-tag-prototype
;;   imenu-create-index-function 'semantic-create-imenu-index
;;   semantic-type-relation-separator-character '(".")
;;   semantic-command-separation-character ";"
;;   semantic-function-argument-separator ","
;;   semantic-function-argument-separation-character ","
   ;; speedbar and imenu buckets name
;;   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
;;   '((type     . "Classes")
;;     (variable . "Variables")
;;     (function . "Methods"))
;;   semantic-symbol->name-assoc-list
   ;; everywhere
;;   (append semantic-symbol->name-assoc-list-for-type-parts
;;           '((include  . "Imports")
;;             (package  . "Package")))
   ;; navigation inside 'type children
;;   senator-step-at-tag-classes '(function variable)
   semantic-debug-parser-source "~/Documents/opx2/devenv/grammars/wisent-pjs-wy.el"
   semantic-debug-parser-class 'semantic-bovine-debug-frame
   semantic-debug-enabled t
   wisent-parse-verbose-flag t
   ;; Remove 'recursive from the default semanticdb find throttle
   ;; since java imports never recurse.
;;   semanticdb-find-default-throttle
;;   (remq 'recursive (default-value 'semanticdb-find-default-throttle))
   )
  
  )

(add-hook 'pjs-mode-hook 'semantic-default-pjs-setup)
