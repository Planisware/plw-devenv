;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT


(defun pjs--regexp-opt-symbol (list &optional prefix)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (cond ((stringp prefix)
	 (concat prefix "\\_<" (regexp-opt list) "\\_>"))
	(t
	 (concat "\\_<" (regexp-opt list) "\\_>"))))

(defvar *pjs-font-lock-debug* nil)

;;; FACES ;;;

;; kernel functions : yellow in italic
(defface pjs-kernel-functions-face
  '((t 
     :inherit font-lock-function-name-face :slant italic))
  "PJS kernel fonts are displayed in italic"
  )

(defvar pjs-kernel-functions-face
  'pjs-kernel-functions-face)

;; functions of the current namespace
;; global variable definution : Red in bold
(defface pjs-namespace-face
  '((t 
     :inherit font-lock-function-name-face :slant italic :weight normal))
  "variable defintion are in bold"
  )

(defvar pjs-namespace-face
  'pjs-namespace-face)

;; global variable definution : Red in bold
(defface pjs-var-definition-face
  '((t 
     :inherit font-lock-variable-name-face :weight bold))
  "variable defintion are in bold"
  )

(defvar pjs-var-definition-face
  'pjs-var-definition-face)


(defface pjs-member-face
  '((t :foreground "#5f9ea0"))
  "PJS member fonts"
  )

(defvar pjs-member-face 'pjs-member-face)

;; symbols : light blue

;; constants
(defconst pjs-font-lock-constants
  (pjs--regexp-opt-symbol
   '("this"
     "super"
     "context"
     "true"
     "false"
     "undefined"
     )))

;; languages keywords
(defconst pjs-font-lock-keywords
  (pjs--regexp-opt-symbol
   '("as"
     "order by"
     "default"
     "default:"
     "on"
     "let"
     "case"
     "class"
     "continue"
     "protected"
     "switch"
     "return"
     "void"
     "for"
     "catch"
     "try"
     "in"
     "method"
     "extends"
     "break"
     "typeof"
     "with"
     "namespace"
     "finally"
     "instanceof"
     "if"
     "group by"
     "applet"
     "else"
     "throw"
     "var"
     "do"
     "where"
     "import"
     "while"
     "function"
     ;;     "wrappable function"
     "new"
     "return values"     
     )))

(defconst *pjs-function-qualifiers*
  (pjs--regexp-opt-symbol
   '("wrappable" "cached")))

(defconst *pjs-function-keyword*
  (format "\\(?:%s\\s-+\\)?\\<function\\>" *pjs-function-qualifiers*))

(defconst *pjs-variable-name*
  "\\(?:\\.\\.\\.\\)?[_a-zA-Z][_a-zA-Z0-9]*")

;; same as pjs method type but with optional new/delete/etc in front of the class name
(defconst *pjs-method-on-keywords*
  (pjs--regexp-opt-symbol
   '("modifybefore"
     "modifybeforesmp"
     "modifyafter"
     "modifyaftersmp"
     "load"
     "loadsmp"
     "new"
     "newsmp"
     "delete"
     "deletesmp")))

(defconst *pjs-method-type*
  ;;  (format "\\(?:%s\\s-+\\)?%s" *pjs-method-on-keywords* *js-type*))
  (format "%s" *js-type*))

;; method definition
(defconst *pjs-method-heading*
  (format "^\\s-*\\<method\\>\\s-+\\(%s\\)\\s-+\\(\\<on\\>\\(?:\\s-+%s\\)?\\)\\s-+\\(%s\\)" *js-function-name* *pjs-method-on-keywords* *pjs-method-type*)
  "Regular expression matching the start of a method header.")

;;method arguments
(defconst *pjs-method-arguments-start*
  (format "\\<method\\>\\s-+\\(%s\\)?\\s-*\\<on\\>\\s-+\\(%s\\)?\\s-*(" *js-function-name* *js-type*))

;; function definition
(defconst *pjs-function-heading*
  (format "^\\s-*\\(%s\\)\\s-+\\(%s\\)" *pjs-function-keyword* *js-function-name*)
  "Regular expression matching the start of a function header.")

;; function arguments
(defconst *pjs-function-arguments-start*
  (format "%s\\s-+\\(%s\\)?\\s-*(" *pjs-function-keyword* *js-function-name*))

(defconst *pjs-arguments-end*
  (format "\\s-*\\(\\<%s\\>\\)\\s-*\\(?::.+\\)?[),]" *pjs-variable-name*))

(defconst *pjs-arguments-type-end*
  (format "\\s-*\\(\\<%s\\>\\)\\s-+\\(\\<%s\\>\\)\\s-*\\(?::.+\\)?[),]" *js-type* *pjs-variable-name*))

;; vars definition with optional type
(defconst *pjs-vars-with-type-regexp* 
  ;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-*\\(%s\\)\\s-+\\(%s\\)\\s-*[=;]" *js-type* *pjs-variable-name*))

(defconst *pjs-vars-no-type-regexp* 
  ;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-+\\(%s\\)\\s-*[=;]" *pjs-variable-name*))

(defconst *pjs-var-in-with-type-regexp* 
  ;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-*\\(%s\\)\\s-+\\(%s\\)\\s-+\\(\\<in\\>\\)" *js-type* *pjs-variable-name*))

(defconst *pjs-var-in-no-type-regexp* 
  ;;  "^.*var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")
  (format "^.*\\<var\\>\\s-+\\(%s\\)\\s-+\\(\\<in\\>\\)" *pjs-variable-name*))

;; class types, plc.somethinf
(defconst *pjs-class-type*
  (format "\\<plc\\>\\.%s" *js-class-name*))

;; new type(
(defconst *pjs-new-type-regexp*
  (format "^.*\\<new\\>\\s-+\\(%s\\)\\s-*(" *js-type*))

;; function starts
(defconst *pjs-function-start-regexp*
  ;;  "\\(function\\|method\\|on new\\|on modifyafter\\|on modifybefore\\|on delete\\)")
  "\\<\\(function\\|method\\)\\>")

;; class definition
(defconst *pjs-class-definition*
  (format "^.*\\<class\\>\\s-+\\(%s\\)" *js-class-name*))

;; symbols between ##
(defconst *pjs-symbols*
  "#[[:alnum:]-_:.]+#")

;; numbers
(defconst *pjs-numbers*
  "-?\\<[0-9]+\\(?:\\.[0-9]+\\)?\\>")

;; match a regexp and check that the character before is not a dot
;; usefull to match language words but not those used as a member or var name
(defun pjs-match-constant (regexp end)
  (re-search-forward-with-test regexp
			       (lambda ()
				 (save-excursion (backward-word)
						 (not (fast-looking-back "."))))
			       end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class members/methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *pjs-namespace-definition*
  (format "^\\s-*\\<namespace\\>\\s-+\\(%s\\);" *js-namespace-name*))

;; TODO : multiple namespaces in one file !

(defvar-resetable *pjs-buffer-namespace* nil 'pjs-compile t)

;; return the current namespace
(defun pjs-current-namespace ()
  (or *pjs-buffer-namespace*      
      (save-excursion
	(goto-char (point-min))
	(setq *pjs-buffer-namespace*
	      (let* ((found (re-real-search-forward *pjs-namespace-definition* nil t))
		     (namespace (when found (downcase (match-string-no-properties 1)))))
		(if found namespace "plw"))))))

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar-resetable *pjs-buffers-class-members-cache* nil 'pjs-compile)
;; contains the master regexp for syntax highlighting
(defvar-resetable *pjs-buffers-class-members-cache-regexp* nil 'pjs-compile)

;; used for autocomplete 
(defvar-resetable *pjs-buffers-class-methods-cache* nil 'pjs-compile)
;; contains the master regexp for syntax highlighting
;;(defvar-resetable *pjs-buffers-class-methods-cache-regexp* 'pjs-compile)

(defun pjs-class-members (namespace class-name)  
  (let ((type (downcase (format "%s.%s" namespace class-name))))
    (or (and *pjs-buffers-class-members-cache* (gethash type *pjs-buffers-class-members-cache*))
	(init-class-members-cache namespace class-name nil))))

;; (defun pjs-class-members-regexp  (namespace class-name)
;;   (let ((type (downcase (format "%s.%s" namespace class-name))))
;;     (or (gethash type *pjs-buffers-class-members-cache-regexp*)
;; 	(init-class-members-cache namespace class-name t))))

(defvar *max-slots-size* 1000)

(defvar *pjs-attributes-filter* nil)

(defun init-class-members-cache (namespace class-name regexp)  
  (unless (hash-table-p *pjs-buffers-class-members-cache*)
    (setq *pjs-buffers-class-members-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-buffers-class-members-cache-regexp*)
    (setq *pjs-buffers-class-members-cache-regexp* (make-hash-table :test 'equal)))
  (when (pjs-configuration-ok)
    (let (list
	  (ht (make-hash-table :test 'equal))
	  (type (downcase (format "%s.%s" namespace class-name)))	  
	  (slots (fi:eval-in-lisp (if *pjs-attributes-filter*
				      (format "(jvs::get-pjs-class-members \"%s\" \"%s\" \"%s\")" namespace class-name *pjs-attributes-filter*)
				    (format "(jvs::get-pjs-class-members \"%s\" \"%s\")" namespace class-name)))))
      (unless (> (length slots) *max-slots-size*)
	(dolist (item slots)
	  ;;    (dolist (item (fi:eval-in-lisp (format "(when (fboundp 'jvs::get-pjs-class-members) (jvs::get-pjs-class-members \"%s\" \"%s\"))" class-name namespace)))
	  (puthash (car item) (second item) ht)
	  (push (car item) list)))
      (puthash type ht *pjs-buffers-class-members-cache*)
      (puthash type (pjs--regexp-opt-symbol list) *pjs-buffers-class-members-cache-regexp*)
      (if regexp
	  list
	ht))))

(defun pjs-class-methods (namespace class-name)
  (unless (hash-table-p *pjs-buffers-class-methods-cache*)
    (setq *pjs-buffers-class-methods-cache* (make-hash-table :test 'equal)))
  (when (pjs-configuration-ok)
    (let ((type (downcase (format "%s.%s" namespace class-name))))
      (or (gethash type *pjs-buffers-class-methods-cache*)
	  (puthash type (loop for item in (fi:eval-in-lisp (format "(jvs::get-method-for-js-class \"%s\" \"%s\")" class-name namespace))
			      collect (downcase item))
		   *pjs-buffers-class-methods-cache*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight namespace functions (current and other namespaces) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-resetable *pjs-namespace-functions-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-functions-regexp-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-variables-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-variables-regexp-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-classes-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-classes-regexp-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-list-cache* nil 'pjs-compile)
(defvar-resetable *pjs-namespace-list-regexp-cache* nil 'pjs-compile)

(defun list-pjs-namespace-functions-regexp (namespace)
  (unless *pjs-namespace-functions-regexp-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-functions-regexp-cache*))

(defun list-pjs-namespace-functions (namespace)
  (unless *pjs-namespace-functions-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-functions-cache*))

(defun list-pjs-namespace-variables-regexp (namespace)
  (unless *pjs-namespace-variables-regexp-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-variables-regexp-cache*))

(defun list-pjs-namespace-variables (namespace)
  (unless *pjs-namespace-variables-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-variables-cache*))

(defun list-pjs-namespace-classes-regexp (namespace)
  (unless *pjs-namespace-classes-regexp-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-classes-regexp-cache*))

(defun list-pjs-namespace-classes (namespace)
  (unless *pjs-namespace-classes-cache*
    (init-pjs-namespace-cache))
  (gethash (upcase namespace) *pjs-namespace-classes-cache*))

(defun search-pjs-current-namespace-functions (end)
  (let* ((namespace (pjs-current-namespace))
	 (ns-functions (list-pjs-namespace-functions-regexp namespace)))
    (when ns-functions
      (re-search-forward (format "\\(?:%s\\.\\)?\\(%s\\)" namespace ns-functions) end t))))

(defun search-pjs-current-namespace-variables (end)
  (let* ((namespace (pjs-current-namespace))
	 (ns-variables (list-pjs-namespace-variables-regexp namespace)))
    (when ns-variables
      (re-search-forward (format "\\(?:%s\\.\\)?\\(%s\\)" namespace ns-variables) end t))))

(defun search-pjs-current-namespace-classes (end)
  (let* ((namespace (pjs-current-namespace))
	 (ns-classes (list-pjs-namespace-classes-regexp namespace)))
    (when ns-classes 
      (re-search-forward (format "\\(?:%s\\.\\)?\\(%s\\)" namespace ns-classes) end t))))

(defun list-pjs-namespaces ()
  (or *pjs-namespace-list-cache*
      (progn (init-pjs-namespace-cache)
	     *pjs-namespace-list-cache*)))

(defun list-pjs-namespaces-regexp ()
  (or *pjs-namespace-list-regexp-cache*
      (progn (init-pjs-namespace-cache)
	     *pjs-namespace-list-regexp-cache*)))

(defun search-pjs-namespace-name (end)
  (let ((ns-list-regexp (list-pjs-namespaces-regexp)))
    (when ns-list-regexp
      (re-search-forward ns-list-regexp end t))))

(defun search-pjs-namespace-functions (end)
  (let ((ns-list-regexp (list-pjs-namespaces-regexp)))
    (when (and ns-list-regexp
	       (re-search-forward (list-pjs-namespaces-regexp) end t)       
	       (fast-looking-at "."))
      (right-char)
      (or (re-search-forward (format "\\=%s" (list-pjs-namespace-functions-regexp (match-string 0))) end t)
	  (set-match-data nil)))))

(defun init-pjs-namespace-cache ()
  (unless (hash-table-p *pjs-namespace-functions-cache*)    
    (setq *pjs-namespace-functions-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-variables-cache*)    
    (setq *pjs-namespace-variables-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-classes-cache*)    
    (setq *pjs-namespace-classes-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-functions-regexp-cache*)    
    (setq *pjs-namespace-functions-regexp-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-variables-regexp-cache*)    
    (setq *pjs-namespace-variables-regexp-cache* (make-hash-table :test 'equal)))
  (unless (hash-table-p *pjs-namespace-classes-regexp-cache*)    
    (setq *pjs-namespace-classes-regexp-cache* (make-hash-table :test 'equal)))  

  (when (pjs-configuration-ok)
    (dolist (ns (fi:eval-in-lisp "(jvs::get-all-namespace-members)"))
      (push (car ns) *pjs-namespace-list-cache*)      
      (puthash (car ns) (pjs--regexp-opt-symbol (mapcar 'car (car (cdr ns))))  *pjs-namespace-variables-regexp-cache*)
      (puthash (car ns) (pjs--regexp-opt-symbol (mapcar 'car (second (cdr ns)))) *pjs-namespace-functions-regexp-cache*)
      (puthash (car ns) (pjs--regexp-opt-symbol (mapcar 'car (third (cdr ns))))  *pjs-namespace-classes-regexp-cache*)
      (puthash (car ns) (car (cdr ns))    *pjs-namespace-variables-cache*)
      (puthash (car ns) (second (cdr ns)) *pjs-namespace-functions-cache*)
      (puthash (car ns) (third (cdr ns))  *pjs-namespace-classes-cache*))
    (setq *pjs-namespace-list-regexp-cache* (pjs--regexp-opt-symbol *pjs-namespace-list-cache*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local vars based on grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pjs-start-of-next-block (tag) ;; (&optional (tag (semantic-current-tag)))
  (catch 'exit
    (let ((next-tag (semantic-find-tag-by-overlay-next)))
      (while next-tag
	(cond ((> (semantic-tag-start next-tag) (semantic-tag-end tag))
	       (throw 'exit (semantic-tag-end tag)))
	      ((memq (semantic-tag-class next-tag) '(function block))
	       (throw 'exit (semantic-tag-start next-tag)))
	      (t
	       (setq next-tag (semantic-find-tag-by-overlay-next (semantic-tag-start next-tag)))))))
    (semantic-tag-end tag)))

(defun search-pjs-local-vars (end)
  (catch 'exit
    (while (< (point) end)
      (let ((current-tag (car (last (semantic-find-tag-by-overlay (point))))))
	(cond ((null current-tag)
	       (let ((next-tag (semantic-find-tag-by-overlay-next)))
		 (if next-tag
		     (goto-char (semantic-tag-start next-tag))
		   (throw 'exit nil))))
	      ((semantic-tag-p current-tag)	       
	       (let ((local-vars (semantic-get-local-variables))
		     ;; end is the start of the next block or the end of the current tag
		     (end (min end
			       (or (pjs-start-of-next-block current-tag)
				   (semantic-tag-end current-tag)))))
		 ;;		 (message "local-vars %s" local-vars)
		 (cond ((re-search-forward (pjs--regexp-opt-symbol (mapcar 'car local-vars)) end t)
			(if (save-excursion (backward-word)
					    (not (fast-looking-back ".")))
			    (throw 'exit (point))
			  (set-match-data nil)))
		       (t
			;; we didn't find anything, go to the end
			(goto-char end)))))
	      (t
	       (message "nothing ??")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight plw kernel functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-resetable *pjs-kernel-functions-cache* nil 'pjs-reset)

(defun list-pjs-kernel-functions ()
  (cond (*pjs-kernel-functions-cache*
	 *pjs-kernel-functions-cache*)
	((pjs-configuration-ok)
	 (setq *pjs-kernel-functions-cache* (format "\\(\\(?:plw\\)?\\.%s\\)\\s-*(" (pjs--regexp-opt-symbol (when (fi::lep-open-connection-p) (fi:eval-in-lisp "(jvs::list-all-js-functions t)"))))))
	(t
	 nil)))

(defun search-pjs-kernel-functions (end)
  (when (list-pjs-kernel-functions)
    (let ((search-pattern (list-pjs-kernel-functions)))
      (re-search-forward search-pattern end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight other namespace functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight "plc" types 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-resetable *pjs-plc-types-cache* nil 'pjs-reset)
(defvar-resetable *pjs-plc-types-to-kernel-cache* nil 'pjs-reset)
(defvar-resetable *pjs-plc-types-cache-regexp* nil 'pjs-reset)

;;(defvar *regexp-elements-limit* 1000)

(defun init-pjs-plc-types-cache ()
  (let* ((functions-list-of-cons (when (pjs-configuration-ok) (fi:eval-in-lisp "(jvs::pjs-list-plc-types)")))
	 (functions-list (mapcar 'car functions-list-of-cons))
	 regexp-list)
    (dolist (sublist (partition-list functions-list *regexp-elements-limit*)) 
      (push (pjs--regexp-opt-symbol sublist "plc\\.") regexp-list))
    (setq *pjs-plc-types-cache-regexp* regexp-list)
    (setq *pjs-plc-types-cache* functions-list)
    (if (hash-table-p *pjs-plc-types-to-kernel-cache*)
	(clrhash *pjs-plc-types-to-kernel-cache*)
      (setq *pjs-plc-types-to-kernel-cache* (make-hash-table :test 'equal)))
    (dolist (l functions-list-of-cons)
      (puthash (car l) (cdr l) *pjs-plc-types-to-kernel-cache*))))

(defun list-pjs-plc-types-to-kernel ()  
  (cond (*pjs-plc-types-to-kernel-cache*
	 *pjs-plc-types-to-kernel-cache*)
	((pjs-configuration-ok)
	 (init-pjs-plc-types-cache)
	 *pjs-plc-types-to-kernel-cache*)
	(t
	 nil)))

(defun list-pjs-plc-types-regexps ()  
  (cond (*pjs-plc-types-cache-regexp*
	 *pjs-plc-types-cache-regexp*)
	((pjs-configuration-ok)
	 (init-pjs-plc-types-cache)
	 *pjs-plc-types-cache-regexp*)
	(t
	 nil)))

(defun list-pjs-plc-types ()  
  (cond (*pjs-plc-types-cache*
	 *pjs-plc-types-cache*)
	((pjs-configuration-ok)
	 (init-pjs-plc-types-cache)
	 *pjs-plc-types-cache*)
	(t
	 nil)))

(defun search-pjs-plc-types (end)
  (let ((found end)
	(start (point)))
    (dolist (regexp (list-pjs-plc-types-regexps))
      (goto-char start)
      (setq found (or (re-search-forward regexp (or found end) t)
		      found)))
    (if (eq found end)
	nil
      found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight "plw" types 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-resetable *pjs-plw-types-cache* nil 'pjs-reset)
(defvar-resetable *pjs-plw-types-cache-regexp* nil 'pjs-reset)

;;(defvar *regexp-elements-limit* 1000)

(defun init-pjs-plw-types-cache ()
  (let* ((functions-list-of-cons (when (pjs-configuration-ok) (fi:eval-in-lisp "(jvs::pjs-list-plw-types)")))
	 (functions-list functions-list-of-cons)
	 regexp-list)
    (dolist (sublist (partition-list functions-list *regexp-elements-limit*)) 
      ;;      (push (format "\\(plw\\.\\|[^.]\\)%s" (replace-regexp-in-string "-" "_?" (pjs--regexp-opt-symbol sublist))) regexp-list))
      (push (replace-regexp-in-string "-" "_?" (pjs--regexp-opt-symbol sublist "\\(plw\\.\\|[^.]\\)")) regexp-list))
    (setq *pjs-plw-types-cache-regexp* regexp-list)
    (setq *pjs-plw-types-cache* functions-list)))

(defun list-pjs-plw-types-regexps ()  
  (cond (*pjs-plw-types-cache-regexp*
	 *pjs-plw-types-cache-regexp*)
	((pjs-configuration-ok)
	 (init-pjs-plw-types-cache)
	 *pjs-plw-types-cache-regexp*)
	(t
	 nil)))

(defun list-pjs-plw-types ()  
  (cond (*pjs-plw-types-cache*
	 *pjs-plw-types-cache*)
	((pjs-configuration-ok)
	 (init-pjs-plw-types-cache)
	 *pjs-plw-types-cache*)
	(t
	 nil)))

(defun search-pjs-plw-types (end)
  (let ((found end)
	(start (point)))
    (dolist (regexp (list-pjs-plw-types-regexps))
      (goto-char start)
      (setq found (or (re-search-forward regexp (or found end) t)
		      found)))
    (if (eq found end)
	nil
      found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight method / members depending on variable type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-member-type (namespace class varname)
  (let ((ht (pjs-class-members namespace class)))    
    (when ht
      (gethash varname ht))))

(defun convert-pjs-type (type)
  (save-match-data
    (cond ((or (null type)
	       (eq type t))
	   nil)
	  ((consp type)
	   type)
	  ((member type *pjs-standard-types*)
	   (cons "plw" type))
	  (t
	   (let ((strings (split-string type "\\.")))
	     (cond ((= (length strings) 1)
		    (cons (pjs-current-namespace) (car strings)))
		   (t
		    (cons (car strings) (second strings)))))))))

;; get the variable type from context :
;; if the variable is local to the function, search a var <type> varname =
;; TODO: if it is a global var of the namespace, check its type
;; TODO: if it is a member of a classe, check the type from the class.
;; we consider we are at the point between the var and its "father"
;; we return a cons (namespace . classname)
(defun get-variable-type-in-context (&optional point )
  (unless point
    (setq point (point)))
  ;; use the parser
  (save-match-data
    (save-excursion
      (goto-char point)
      (cond ((fast-looking-back "context")
	     (convert-pjs-type "plc.contextopx2"))
	    ((when (fast-looking-back ")") 	    ;; (stuff as class)
	       (backward-sexp)
	       (re-search-forward (format "(\\s-*.+\\s-+as\\s-+\\<\\(%s\\)\\>)" *js-type*) (line-end-position) t))
	     (convert-pjs-type (match-string-no-properties 1)))
	    ((re-search-backward (format "\\<%s\\>" *pjs-variable-name*) (line-beginning-position) t)
	     (let ((varname (downcase (match-string-no-properties 0)))  	      
		   var-tag)
	       (cond ((fast-looking-back ".")
		      ;; we have a dot, try to get the type of the "father"
		      (let ((father-type (get-variable-type-in-context (match-beginning 0))))
			(when father-type
			  (get-member-type (car father-type) (cdr father-type) varname))))
		     ;; this of a method
		     ((string= (downcase varname) "this")
		      (let ((current-tag (car (semantic-find-tag-by-overlay))))
			(when (and current-tag
				   (eq (semantic-tag-class current-tag) 'function))
			  (convert-pjs-type (semantic-tag-get-attribute current-tag :on-class)))))
		     ;; local variable of the function, try to get the type
		     ((setq var-tag (block tag
				      (dolist (tag (semantic-find-tags-by-name varname (semantic-something-to-tag-table (semantic-get-local-variables))))
					(when (semantic-tag-get-attribute tag :type)
					  (return-from tag tag)))))
		      (convert-pjs-type (semantic-tag-get-attribute var-tag :type)))
		     ;; TODO : namespace var with type
		     ;; ((let ((list-of-var (list-pjs-namespace-variables (pjs-current-namespace))))
		     ;; 	(when list-of-var
		     ;; 	  (dolist (_var list-of-var nil)
		     ;; 	    (when (eq (compare-strings (car _var) nil nil varname nil nil t) t)
		     ;; 	      (if (and (cdr _var)
		     ;; 		       (not (string-equal (cdr _var) "")))
		     ;; 		  (return (convert-pjs-type (cdr _var)))
		     ;; 		(return nil)))))))
		     (t
		      ;;		 (message "nothing found :(")
		      nil))))))))

(defconst *pjs-vars-with-members-or-methods*
  (format "\\.\\<\\(%s\\)\\>" *pjs-variable-name*))
;;  (format "\\<\\(%s\\)\\>\\.\\<\\(%s\\)\\>\\(?:(\\)?" *pjs-variable-name* *pjs-variable-name*))

(defconst *pjs-standard-types*
  '("string"
    "boolean"
    "number"
    "vector"
    "array"
    "hashtable"))

(defun search-vars-with-members-or-methods (end)
  (let ((match-data (match-data))
	continue)
    (or (catch 'exit
	  (while (re-search-forward *pjs-vars-with-members-or-methods* end t)
	    (save-match-data
	      (let* ((member   (downcase (match-string-no-properties 1)))
		     (var-type (get-variable-type-in-context (match-beginning 0))))
		(when var-type
		  (let* ((members (pjs-class-members (car var-type) (cdr var-type)))
			 (methods (pjs-class-methods (car var-type) (cdr var-type))))
;;		    (message "member is %s members are %s methods are %s %s %s" member members methods (gethash member members :unknown) (member member methods))
		    (when (or (and members (not (eq (gethash member members :unknown) :unknown)))
			      (member member methods))
		      (throw 'exit (point)))))))))
	(progn (set-match-data nil)
	       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax hightlighting definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-pjs-syntax-highlighting ()
  ;; syntax highlighting for pjs
  
  (setq font-locks nil)

  (setq font-lock-verbose t)

  ;; list of font-lock-keywords in the right order

  ;; LAST RULE APPLIED HERE
  ;; highlight "plc" types
  (push (cons 'search-pjs-plc-types font-lock-type-face) font-locks)

  ;; highlight "standard" types
  (push (cons 'search-pjs-plw-types font-lock-type-face) font-locks)
  
  ;; hightlight namespace types
  ;; Namespace classes
  (push (cons 'search-pjs-current-namespace-classes font-lock-type-face) font-locks)  
  
  ;; Functions defined in buffers  
  (push (list 'search-pjs-current-namespace-functions 1 font-lock-function-name-face) font-locks)

  ;; Kernel functions
  ;;  (push (cons 'search-pjs-kernel-functions pjs-kernel-functions-face) font-locks)
  (push (list 'search-pjs-kernel-functions 1 pjs-kernel-functions-face) font-locks)

  ;; namespace functions
  (push (cons 'search-pjs-namespace-functions font-lock-function-name-face) font-locks)
  
  ;; keywords
  (push (cons (lambda (end) (pjs-match-constant pjs-font-lock-keywords end)) font-lock-keyword-face) font-locks)
  
  ;; constants
  (push (cons (lambda (end) (pjs-match-constant pjs-font-lock-constants end)) font-lock-constant-face) font-locks)
  
  ;; Variables in the function 
  ;;  (push (cons 'search-function-local-vars font-lock-variable-name-face) font-locks)
  (push (cons 'search-pjs-local-vars font-lock-variable-name-face) font-locks)

  ;; members and methods based on type of var
  (push (list 'search-vars-with-members-or-methods 1 pjs-member-face) font-locks)

  ;; Namespace vars
  (push (cons 'search-pjs-current-namespace-variables pjs-var-definition-face) font-locks)

  ;; Namespace names
  (push (cons 'search-pjs-namespace-name pjs-namespace-face) font-locks) 

  ;; Variable definitions with type
  ;;  (push (list *pjs-vars-with-type-regexp* 1 font-lock-type-face) font-locks)
  (push (list *pjs-vars-with-type-regexp* 2 pjs-var-definition-face) font-locks)

  ;; variable definition without type
  (push (list *pjs-vars-no-type-regexp* 1 pjs-var-definition-face) font-locks)

  ;; var in with type
  ;;  (push (list *pjs-var-in-with-type-regexp* 1 font-lock-type-face) font-locks)
  (push (list *pjs-var-in-with-type-regexp*
	      '(2 pjs-var-definition-face)
	      '(3 font-lock-keyword-face))
	font-locks)
  ;;  (push (list *pjs-var-in-with-type-regexp* 3 font-lock-keyword-face) font-locks)

  ;; var in without type
  (push (list *pjs-var-in-no-type-regexp*
	      '(1 pjs-var-definition-face)
	      '(2 font-lock-keyword-face))
	font-locks)
  ;;  (push (list *pjs-var-in-no-type-regexp* 2 font-lock-keyword-face) font-locks)  

  ;; class definitions
  (push (list *pjs-class-definition* 1 font-lock-type-face) font-locks)
  
  ;; symbols
  (push (list *pjs-symbols* 0 font-lock-preprocessor-face) font-locks)  

  ;; numbers
  (push (list *pjs-numbers* 0 font-lock-preprocessor-face) font-locks)  
  
  ;; Function definition
  (push  (list *pjs-function-heading*
	       '(1 font-lock-keyword-face)
	       '(2 font-lock-function-name-face))
	 font-locks)
  ;;  (push (list *pjs-function-heading* 2 font-lock-function-name-face) font-locks)
  
  ;; Function arguments
  (push (list
	 (concat *pjs-function-arguments-start*)
	 ;; simple arguments
	 (list *pjs-arguments-end*
	       '(progn (backward-char)
		       (min (or (ignore-errors (scan-sexps (point) 1))
				(line-end-position))
			    (line-end-position)))
	       '(re-search-backward "(" (line-beginning-position) t)
	       '(1 pjs-var-definition-face))
	 ;; arguments with types
	 (list *pjs-arguments-type-end*
	       '(progn (backward-char)
		       (min (or (ignore-errors (scan-sexps (point) 1))
				(line-end-position))
			    (line-end-position)))
	       '(end-of-line)
	       '(1 font-lock-type-face)
	       '(2 pjs-var-definition-face))
	 )
	font-locks)
  
  ;; Method definition
  (push (list *pjs-method-heading*
	      '(1 font-lock-function-name-face)
	      '(2 font-lock-keyword-face)
	      )
	font-locks)
  ;;  (push (list *pjs-method-heading* 2 font-lock-keyword-face) font-locks)
  ;;  (push (list *pjs-method-heading* 3 font-lock-type-face) font-locks)  
  ;; Method arguments
  (push (list
	 (concat *pjs-method-arguments-start*)
	 ;; simple arguments
	 (list *pjs-arguments-end*
	       '(progn (backward-char)
		       (min (or (ignore-errors (scan-sexps (point) 1))
				(line-end-position))
			    (line-end-position)))
	       '(re-search-backward "(" (line-beginning-position) t)
	       '(1 pjs-var-definition-face))
	 ;; arguments with types
	 (list *pjs-arguments-type-end*
	       '(progn (backward-char)
		       (min (or (ignore-errors (scan-sexps (point) 1))
				(line-end-position))
			    (line-end-position)))
	       '(end-of-line)
	       '(1 font-lock-type-face)
	       '(2 pjs-var-definition-face))
	 )
	font-locks)
  
  ;; FIRST RULE APPLIED HERE
  
  ;; comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")

  (when *pjs-font-lock-debug*
    (load (format "%s/devenv/el/pjs-mode-debug.el" *opx2-network-folder-work-path*)))

  (if *pjs-font-lock-debug*      
      (set (make-local-variable 'font-lock-defaults)
	   (list font-locks
		 nil ;; fontify strings and comments
		 t   ;; case insensitive fontifying
		 nil
		 nil
		 (cons 'font-lock-fontify-region-function 'pjs-font-lock-default-fontify-region)
		 ))
    (set (make-local-variable 'font-lock-defaults)
	 (list font-locks
	       nil ;; fontify strings and comments
	       t   ;; case insensitive fontifying
	       nil
	       nil
	       )))
  

  (font-lock-mode)
  ;; build the cache before fontifying 
  ;;  (jit-lock-register 'build-local-vars-cache)

  ;; fontify all the things
  ;;  (syntax-propertize (point-max))
  )

(setq font-lock-verbose t)

(defun force-syntax-highlighting ()
  (interactive)
  (font-lock-fontify-buffer))

(defun pjs-reset-cache-on-save ()
  (js-reset-vars 'pjs-save)
  (semantic-force-refresh))

(defun pjs-reset-cache-on-compile ()
  (js-reset-vars 'pjs-compile)
  (semantic-force-refresh))
