;;* 
;;  COPYRIGHT (C) PLANISWARE 2016-05-27
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************
(defun update-db (message)
  " @PURPOSE Updates the database using message content
   It opens the database and sets the global var *current-db-string*
  @ARGUMENTS
    $message String that is the database name
  @RESULT Does not matter
  @NB Callback function in menu defintion"
  (let ((db-buffer (find-file (concat *opx2-network-folder-work-path* "/database.ini"))))
    (save-excursion 
      (beginning-of-buffer)
      (re-search-forward "\*intranet-database\*" nil t)
      (fi:beginning-of-defun)
      (down-list)
      (let ((start (progn (forward-sexp 2) (point)))
	    (end (progn (forward-sexp 1) (point))))
	(delete-region start end)
	(insert " \"")
	(insert message)
	(insert "\"")
	)
      (save-buffer)
      (setq *current-db-string* message)
      (set-menubar-dirty-flag))
    (kill-buffer db-buffer)))

(defun opx2-add-documentation ()
  ;; Usable with M-x, displaying 
  (interactive) 
  (save-excursion
    (let ((message (message "\n\" @PURPOSE\n  @ARGUMENTS\n")))
      (fi:beginning-of-defun)
      (down-list)
      (forward-sexp 2) ;; go after function name
      (let* ((start (point))
	     (end 
	      (progn (forward-sexp)
		     (point)))
	     (arg-list (car (read-from-string (buffer-substring start end))))
	     (mode  " "))
	(dolist (arg arg-list)
	  (case (type-of arg)
	    (symbol
	     (case arg
	       ('NIL
		)
	       (&key
		(setq mode " [key]"))
	       (&optional
		(setq mode " [optional]"))
	       (&rest
		(setq mode " [rest]"))
	       (&aux
		(return))
	       (otherwise
		(setq message (concat  message "    $"  (format "%S" arg) mode "\n")
		      ))))
	    (cons
	     (setq message (concat message "    $" (format "%S" (car arg)) mode "\n    [Default]" (format "%S" (second arg)) "\n")))))
	
	(setq message (concat message "  @RESULT\n  @NB\"\n" ))
	
	
	;; check that no string comes righet after function definition
	(let* ((start (point))
	       (end (save-excursion 
		      (forward-sexp)
		      (point))))
	  (unless (stringp (car (read-from-string (buffer-substring start end))))
	    (insert message)))))))

(defvar *sources-dir-cache* nil)

(defun get-sources-dir (full-path short-path)
  (or *sources-dir-cache*
      (setq *sources-dir-cache* (get-sources-subdir full-path short-path))))

(defun get-sources-subdir (full-path short-path)
  "computes strings that are relative paths to subdirectories
   of the dev root directory"
  (let ((res-lst nil))
    (dolist (file (directory-files full-path))
      (unless (or (equal file ".")
		  (equal file ".."))
	(let ((full-file-name (concat full-path
				      file
				      "/"))
	      (rel-file-name (concat  short-path
				      file
				      "/")))
	  (when (file-directory-p full-file-name)
	    (push rel-file-name res-lst)
	    (setq res-lst
		  (concatenate 'list 
			       (get-sources-subdir full-file-name rel-file-name)
			       res-lst))))))

    res-lst))


(defun opx2-create-test-template()
  "Creates a test template from an open buffer"
  (interactive)
  (let* ((template-path (concat *opx2-network-folder-work-path* "/devenv/test-template.lisp"))
	 (patch-buffer (current-buffer))
	 (patch-name (subseq (buffer-name patch-buffer) 0 (- (length (buffer-name patch-buffer)) 5)))
	 (test-patch-name (concat *opx2-network-folder-work-path*
				  "/kernel/tests/dev/test-"
				  patch-name
				  ".lisp")))
    (cond ((file-exists-p test-patch-name)
	   ;; test fix exists do not replace
	   (find-file test-patch-name))
	  ((file-exists-p template-path)
	   (let ((template-buffer (find-file template-path)))
	     (when template-buffer
	       (setq template-buffer (write-file test-patch-name))
	       (beginning-of-buffer)
	       (while (re-search-forward "#scyourpatch#" nil t)
		 (replace-match patch-name nil nil)))))
	  (t ;; no template provided
	   ))))


(defun opx2-compare-redefinition()
  (interactive)
;;;; VERSION: sc3817.lisp 3.2
  (let ((function-name ""))
    ;; look for the VERSION tag 
    (when (save-excursion 
	    (re-search-backward   "[ /]\\([^ ]*.lisp\\)")
	    )
      
      (let* ((default-major-mode 'fi:common-lisp-mode)
	     (patch-name (match-string 1))
	     (old-buff (current-buffer))
	     (original-subbuff (get-buffer-create (format "CMP-original %s" patch-name)))
	     (redef-subbuff (get-buffer-create (format "CMP-redefinition %s" (buffer-name (current-buffer)))))
	     (paths-list (get-sources-dir (concat *opx2-network-folder-work-path* "/kernel/") "/kernel/"))) ;; look only into kernel dir
	;; search original source file that contains the redefined function
	(dolist (path-part paths-list)
	  (when (file-exists-p (concat *opx2-network-folder-work-path* path-part patch-name))
	    (setq original-file (concat *opx2-network-folder-work-path* path-part patch-name))))
	(if original-file
	    (progn 
	      ;;process redefinition
	      (save-excursion
		(let* ((end (progn (end-of-defun)(point)))	
		       (start (save-excursion (beginning-of-defun)(point)))
		       (redefined-body (buffer-substring start end)))
		  
		  
		  (with-current-buffer redef-subbuff
		    (normal-mode)
		    (insert-buffer-substring old-buff start end)
		    (set-window-buffer (selected-window) redef-subbuff))))
	      ;;process original function
	      
	      (let (start end f-name
			  (original-buffer (generate-new-buffer "*TMP-CMP*.lisp")))
		(save-excursion (fi:beginning-of-defun)
				(setq start (point))
				(if (re-search-forward "\\(.+\\)" nil t)
				    (progn 
				      (setq f-name (regexp-quote (match-string 1)))
				      )
				  (progn
				    (fi:beginning-of-defun)
				    (down-list)
				    (forward-sexp)
				    (setq start (point))
				    (forward-sexp)
				    (setq end (point))
				    (setq f-name  (concat "[ ]*?" (regexp-quote (buffer-substring start end)) "[ ]*?("))))

				)

		(with-current-buffer original-buffer
		  (insert-file-contents original-file)

		  (save-excursion 
		    (beginning-of-buffer)

		    (if (re-search-forward f-name  nil t)  
			(let* ((start (save-excursion (fi:beginning-of-defun)(point)))
			       (end (save-excursion (end-of-defun)(point)))
			       (original-body (buffer-substring start end)))

			  (with-current-buffer original-subbuff
			    (normal-mode)
			    (insert-buffer-substring original-buffer start end)
			    ))
		      (progn
			(kill-buffer original-subbuff)
			(kill-buffer redef-subbuff )
			(kill-buffer original-buffer)
			(message-box (format "Could not locate symbol %s in file %s " f-name patch-name))))))
		(kill-buffer original-buffer))
	      (with-current-buffer original-subbuff
		(font-lock-fontify-buffer))
	      (with-current-buffer redef-subbuff
		(font-lock-fontify-buffer))
	      (ediff-buffers original-subbuff redef-subbuff))
	  
	  (progn
	    ;;warn the user it sucked
	    (kill-buffer original-subbuff)
	    (kill-buffer redef-subbuff)
	    (message-box (format "Could not locate patch %s" patch-name))
	    ))))))

(defun opx2-redefine-function ()
  "Fetch (in-package) and $Id: from a cvs file"
  (interactive)
  (let* ((location (buffer-file-name (current-buffer)))
	 done package version absolute-prefix-pos
	 revision)
    
    
    ;;building relative location 
					;Unix separator
    (dotimes (idx (length location))
      (when (eq (aref location idx)
		?\\)
	(aset location idx ?\/)))

    ;;find relatibe path using regexps
    (setq absolute-prefix-pos (string-match (concat ".*" (regexp-quote *opx2-network-folder-work-path*) "\\(.*\\)" ) location))
    (when absolute-prefix-pos
      (setq location (match-string 1 location)))
    
    (save-excursion
      ;;go back until we found a (in-package )
      (when (re-search-backward "^\\s-*(\\([a-zA-Z0-9:\"_\\-]*:\\)?in-package \\([^)\n]*\\)")
	(if (match-string 2)
	    (setq package (match-string 2))
	  (setq package (match-string 1))))

      ;;from the beginning look for a $Id: line
      (beginning-of-buffer)
      (when (re-search-forward "^;+\\s-*VERSION\\s-*:\\s-*\\$Id:\\s-*\\(.*\\),v\\s-*\\([0-9.]+\\)" nil t)
	(setq revision (match-string 2)
	      version (concat (match-string 1) " " (match-string 2)))))

    (let* ((end (save-excursion (end-of-defun) (point)))
	   (start (save-excursion
		    (fi:beginning-of-defun) (point))))
      (kill-new (concat "(in-package " package ")\n\n;;;; VERSION: " (if absolute-prefix-pos
									 (concat location " " revision)
								       version)
			"\n;;Changed: \n" (buffer-substring start end))))))

(defun add-redefinition-shortcuts ()
  ;;Redefinition management
  (define-key fi:common-lisp-mode-map "\C-or" 'opx2-redefine-function)
  (define-key fi:common-lisp-mode-map "\C-od" 'opx2-add-documentation)
  (define-key fi:common-lisp-mode-map "\C-ov" 'opx2-compare-redefinition)
  (define-key fi:common-lisp-mode-map "\C-ot" 'opx2-create-test-template)
  (define-key fi:common-lisp-mode-map "\C-cn" 'fi:lisp-find-next-definition)
  )

(add-hook 'fi:common-lisp-mode-hook 'add-redefinition-shortcuts)


(defvar *restricted-commit-directories* '("patches-dev" "patches"))

(defun is-a-restricted-directory (full-path)
  (block nil
    (dolist (rp *restricted-commit-directories*)
      (if (string= (substring full-path (max (- (length full-path) (length rp) 1) 0) (- (length full-path) 1)) rp)
	  (return t)))))

;;new
(defun find-doc-string-when-needed()
  (let ((ext (file-name-extension (file-name-nondirectory (buffer-file-name))))
	(ret nil)
	(case-fold-search t)) ;;case insensitive search
    (cond ((equal ext "lisp")
	   (save-excursion
	     (beginning-of-buffer)
	     (if (re-search-forward "^;+\\s-*DOC\\s-*:?\\s-*\\(.*\\)" nil t)
		 (setq ret (list (match-string 1)))
	       (error "No doc string was found")))))
    
    (unless ret	  
      (setq ret (list (read-string "Description:"))))
    ret))

(defun message-raw (text)
  (message (replace-regexp-in-string "%" "%%" text)))

;; new find definition which tries to find the definition in the file if no point is returned
;; also manages ojs files
(defun fi::show-found-definition (thing pathname point n-more
					&optional other-window-p pop-stack next)
  (if pathname
      (if (equal (downcase pathname) "top-level")
	  (message
	   "%s was defined somewhere at the top-level, %d more definitions"
	   thing n-more)
	(let ((mess "")
	      (xb nil)
	      (pathname (fi::ensure-translated-pathname pathname)))
	  (when fi:filename-frobber-hook
	    (setq pathname (funcall fi:filename-frobber-hook pathname)))
	  (ring-insert lep::show-def-marker-ring (point-marker))
	  (setq xb (get-file-buffer pathname))
	  (if other-window-p
	      (find-file-other-window pathname)
	    (find-file pathname))
	  ;; rfe10778. why is the set-mark necessary?
	  ;; (if xb (set-mark (point)))
	  ;;	  (message "this is %s point is %s" thing point)
	  (if (null point)
	      (try-to-find-in-file thing pathname)
	    (progn
	      (goto-char (1+ point))
	      ;; rfe10778. why is the set-mark necessary?
	      ;; (if (not xb) (set-mark (point)))
	      ))
	  (cond ((eq n-more 0)
		 (if (lep::meta-dot-from-fspec)
		     (message (concat mess "%ss of %s")
			      (lep::meta-dot-what) (lep::meta-dot-from-fspec))
		   (message (concat mess "No more %ss of %s")
			    (lep::meta-dot-what) (or (lep::meta-dot-string) thing))))
		(n-more
 		 (message-raw (concat mess (format "%s of %s. %d more %ss"					      
					      (capitalize (lep::meta-dot-what))
					      thing
					      n-more
					      (lep::meta-dot-what)
					      next)
				      (if next (format ", next is %s" next) "."))
			      )))
;;;			  (or (lep::meta-dot-from-fspec) thing))))
	  (when pop-stack (fi::pop-metadot-session))))
    (message "cannot find file for %s" thing)))

(when (and (boundp 'plw-source-methods) plw-source-methods)
  ;; new argument : next def
  (defun fi::lisp-find-definition-common (something other-window-p
						    &optional what from-fspec)
    (when (not (fi::lep-open-connection-p))
      (error "connection to ACL is down--can't find tag"))
    (message "Finding %s for %s..." (or what "definition") something)
    (fi::push-metadot-session
     (or what "definition")
     something
     from-fspec
     (fi::make-complex-request
      (scm::metadot-session
       :package (fi::string-to-keyword (fi::package))
       :type t				; used to be (or type t), but
					; `type' is not bound in this
					; context
       :fspec something)
      ((something other-window-p what from-fspec)
       (pathname point n-more what next)
       (fi::show-found-definition (or what
				      lep::meta-dot-string
				      (if (symbolp something)
					  (symbol-name something)
					something))
				  pathname
				  point n-more other-window-p
				  (eq 0 n-more)
				  next)
       (if (= 0 n-more) (fi::pop-metadot-session)))
      (() (error)
       (when (fi::pop-metadot-session)
	 (fi::show-error-text "%s" error))))))

  (defun fi:lisp-find-next-definition ()
    "Continue last tags search, started by fi:lisp-find-definition.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
    (interactive)
    (message "Finding next %s..." (lep::meta-dot-what))
    (if (not (lep::meta-dot-session)) (error "No more definitions"))
    (fi::make-request-in-existing-session
     (lep::meta-dot-session)
     (:next)
     (() (pathname point n-more what next)
      (fi::show-found-definition (or what (lep::meta-dot-string)) pathname point n-more
				 nil (eq 0 n-more) next))
     (() (error)
      (when (fi::pop-metadot-session)
	(fi::show-error-text "%s" error)))))

  )

(defconst *defun-words*
  (regexp-opt
   '("defun"
     "defmethod"
     "defmacro"
     "defglobal"
     "defun-ajax"
     "defWfun"
     "defgeneric"
     "deflocal")))

;;(defconstant FORBIDEN_CHARS "-=./\?@*")
(defconst FORBIDEN_CHARS_REGEXP "[-=./?@*]")

(defun class-name-regexp (str)
  (let ((res "")
	(i 0))
    (while (< i (length str))
      (let ((next (string-match FORBIDEN_CHARS_REGEXP str i)))
	(cond (next
	       (setq res (concat res (substring str i next) "_?"))
	       (setq i (1+ next)))
	      (t
	       (setq res (concat res (substring str i)))
	       (setq i (length str))))))
    res))

(defun try-to-find-in-file (thing pathname)
  ;; try to find the definition "manually" in the file
  ;;  (message "thing is %s and is a %s" thing (cond ((stringp thing) "string") ((symbolp thing) "symbol")))
  ;; get the name of the function or the name of the method
  (let* ((split (split-string thing ":+" t))
	 (function-name (car (last split)))
	 (method-name (let ((split-method (split-string thing "\\."))) (when (>= (length split-method) 2) (second split-method))))
	 )
    ;; we return a value for message
    (if 
	(cond ((equal (substring pathname -3 nil) "ojs")
	       ;; try to find in ojs file
	       (goto-char (point-min))
	       ;;	       (message "Searching %s" (concat "\\(function\\|method\\)[ \t]+\\_<" function-name "\\_>"))
	       (re-search-forward (concat "^\\s-*\\(function\\|method\\)\\s-+\\_<" function-name "\\_>") (point-max) t))
	      ((and (equal (substring pathname -3 nil) "pjs")
		    (string-prefix-p "METHOD" function-name t))
	       (let* ((split (split-string function-name "\\."))
		      (method-name (when (>= (length split) 3) (second split)))
		      (class       (cond ((= (length split) 3) (class-name-regexp (third split))) ((= (length split) 4) (class-name-regexp (fourth split)))))
		      (namespace   (when (= (length split) 4) (third split))))
		 (goto-char (point-min))
		 (cond ((and method-name class namespace)
			(or 
			 (re-search-forward (format "^\\s-*method\\s-+\\<%s\\>\\s-+on\\s-+\\<%s\\>" method-name class) (point-max) t) ;; method name on class
			 (re-search-forward (format "^\\s-*method\\s-+\\<%s\\>\\s-+on\\s-+\\<%s\\.%s\\>" method-name namespace class) (point-max) t))) ;; method name on namespace.class
		       ((and method-name class)
			(or 
			 (re-search-forward (format "^\\s-*method\\s-+\\<%s\\>\\s-+on\\s-+\\<%s\\>" method-name class) (point-max) t) ;; method name on class
			 (re-search-forward (format "^\\s-*method\\s-+\\<%s\\>\\s-+on\\s-+\\<pl[wc]\\.%s\\>" method-name class) (point-max) t))))));; method name on plc\plw.class
	      ((and (equal (substring pathname -3 nil) "pjs")
		    ;; try to find in pjs file : try without the package, then with it
		    (let* ((split (split-string function-name "\\."))
			   (short-function-name (when (= (length split) 2) (second split))))
		      (goto-char (point-min))
		      (or 
		       (re-search-forward (format "^\\s-*\\(%s\\)?\\s-*\\(function\\|method\\)\\s-+\\_<%s\\_>" *pjs-function-qualifiers* (or short-function-name function-name)) (point-max) t)
		       (when short-function-name (re-search-forward (format "^\\s-*\\(%s\\)?\\s-*\\(function\\|method\\)\\s-+\\_<%s\\_>" *pjs-function-qualifiers* function-name) (point-max) t))))))
	      ((or (equal (substring pathname -3 nil) "lsp") (equal (substring pathname -4 nil) "lisp"))
	       ;; try to find in lisp file	       
	       (goto-char (point-min))
	       (or 
		;; try to find the defun wihtout the package
		(progn ;;(message "Searching %s" (concat "^(" *defun-words* "[ \t]+\\_<" function-name "\\_>[ \t(]+"))
		  (if method-name
		      (re-search-forward (concat "^(" *defun-words* "[ \t(]+\\_<\\(\\w+:+\\)?" method-name "\\_>[ \t(]+") (point-max) t)
		    (re-search-forward (concat "^(" *defun-words* "[ \t(]+\\_<\\(\\w+:+\\)?" function-name "\\_>[ \t(]+") (point-max) t)))))
	      (t
	       (message "Unknown filetype %s" pathname)
	       nil)
	      )
	;; we found it  so no message
	(progn 
	  ;; go to the beginning of the line
	  (beginning-of-line)
	  "")
      (progn
	(message "The definition of %s is somewhere in this file!" thing)
	(goto-char (point-min))))))


;; new : list all methods of a defgeneric in a buffer
(defun opx2-list-methods (&optional fspec)
  ;;  (interactive (fi::get-default-symbol "List methods of" nil nil t))
  (interactive (fi::get-default-symbol "List methods of" nil nil t))
  (message "Finding methods of %s..." fspec)
  (if (fi:eval-in-lisp "(if (fboundp 'opx2-lisp::list-methods) t nil)")
      (lep::list-fspecs-common fspec
			       'opx2-lisp::list-methods
			       "Cannot find the methods of %s"
			       "caller")
    (message "list-methods function not found")))  


;;;======================== tab list display ===============================
(defun display-list-in-current-buffer (list header-list sort-list )
  (setq tabulated-list-format  header-list)
  (setq tabulated-list-entries list)
  (setq tabulated-list-sort-key sort-list)
  (setq tabulated-list-use-header-line t)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun print-value (val)
  (or (and (stringp val) (substring-no-properties val
						  (if (= (aref val 0) 34)
						      1
						    0)
						  (if (= (aref val (1- (length val))) 34)
						      (1- (length val))
						    (length val))))
      (and (symbolp val) (symbol-name val))
      (and val (format "%s" val))
      ""))

;;;======================== redefinitions ==================================

;; use opx2-lisp::who-calls
(defun fi:list-who-calls (&optional fspec)
  "List all the callers of FSPEC.  `List' means to show them in a buffer in
definition mode.  The source for each definition can be easily found via
key bindings in definition mode.  The default FSPEC is taken from the text
surrounding the point.  fi:package is used to determine from which Common
Lisp package the operation is done.  In a subprocess buffer, the package is
tracked automatically.  In source buffer, the package is parsed at file
visit time."
  (interactive (fi::get-default-symbol "List who calls" nil nil t))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding callers of %s..." fspec)
  (lep::list-fspecs-common fspec
			   (if (fi:eval-in-lisp "(if (fboundp 'opx2-lisp::who-calls) t nil)")			       
			       'opx2-lisp::who-calls
			     'lep::who-calls)
			   "Cannot find the callers: %s"
			   "caller"))

(defvar *definitions-list-headers* (vector '("Name"        80 t)
					   '("Source file" 50 t)))

(defvar *definitions-list-sort* '("Source file"))
;;(defvar *definitions-list-sort* nil)


;; use a tabulated list to display things nicely
(defun lep:display-some-definitions (xpackage buffer-definitions
					      fn-and-arguments
					      &optional buffer-name)
  (let ((buffer (get-buffer-create (or buffer-name "*definitions*")))
	(i 0)
	new-list)
    (fi::goto-definitions-buffer
     buffer
     'lep::definition-mode-saved-window-configuration)
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq truncate-lines t)		;smh 22jul94
      (fi:opx2-definition-mode)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (setq lep::definitions (mapcar (lambda (l)
				       (if (third l)
					   (format "%s::%s" (car l) (third l))
					 (car l)))
				     buffer-definitions))
      (setq lep::definition-types (mapcar 'second buffer-definitions))
      (setq lep::definition-other-args (mapcar 'third buffer-definitions))
      (setq lep::definition-finding-function fn-and-arguments)
      (setq fi:package xpackage)
      (dolist (item buffer-definitions)
	(push (list i
		    (vector (print-value (car item))
			    (print-value (second item))))
	      new-list)
	(setq i (1+ i)))
      (display-list-in-current-buffer new-list
				      *definitions-list-headers*
				      *definitions-list-sort*)
      (goto-char (point-min)))))

;; use the index provided by the tabulation id 
(defun fi:definition-mode-goto-definition ()
  "Find the definition associated with the entry on the current line.  This
uses the same mechanism as fi:lisp-find-definition, using dynamic
information in the Common Lisp environment."
  (interactive)
  (message "Finding%s definition..."
	   (if lep::inverse-definitions " inverse" ""))
  ;;  (let* ((n (count-lines (point-min)
  ;;			 (save-excursion (beginning-of-line) (point))))
  (let* ((n (tabulated-list-get-id))
	 (buffer (current-buffer))
	 (def (nth n lep::definitions))
	 (other (nth n lep::definition-other-args))
	 (type (nth n lep::definition-types)))
    (when (and (not (equal type '(nil))) lep::definition-finding-function)
      (apply (car lep::definition-finding-function)
	     def type buffer
	     (append other (cdr lep::definition-finding-function))))))

;;; new, based on fi:definition-mode
(define-derived-mode fi:opx2-definition-mode tabulated-list-mode "Definitions" ()
  "A major mode for viewing definitions of objects defined in the Common
Lisp environment.  The definitions are put in a buffer called
*definitions*, and each line contains the name and type of the definition.
The type is one of:

	:operator	for functions, methods, generic functions
				and macros,
	:type		for classes (types),
	:setf-method	for setf methods, or
	:variable	for constants and variables.

Definition mode is used by other tools, such as the changed-definition
commands, fi:list-who-calls as well as fi:list-buffer-definitions.

The keymap for this mode is bound to fi:definition-mode-map:

<font face=\"Courier New\">\\{fi:definition-mode-map}</font>
Entry to this mode runs the fi:definition-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:opx2-definition-mode)
  (setq mode-name "OPX2 definition Mode")

  (make-local-variable 'truncate-lines)
  (setq truncate-lines t)
  (fi::definition-mode-fix-buffer)

  (make-local-variable 'lep::definitions)
  (make-local-variable 'lep::definition-types)
  (make-local-variable 'lep::definition-other-args)
  (make-local-variable 'lep::definition-finding-function)
  (make-local-variable 'lep::inverse-definitions)

  (setq lep::inverse-definitions nil)	;In fsf Emacs a local var remains
					;unbound unless explicitly set.
  
  (define-key fi:opx2-definition-mode-map "\C-_"  'fi:definition-mode-undo)
  (define-key fi:opx2-definition-mode-map "."     'fi:definition-mode-goto-definition)
  (define-key fi:opx2-definition-mode-map "\r"    'fi:definition-mode-goto-definition)
  (define-key fi:opx2-definition-mode-map "\C-c"  (make-sparse-keymap))
  (define-key fi:opx2-definition-mode-map "\C-c." 'fi:definition-mode-goto-definition)
  (define-key fi:opx2-definition-mode-map "n"     'fi:definition-mode-goto-next)
  (define-key fi:opx2-definition-mode-map "p"     'fi:definition-mode-goto-previous)
  (define-key fi:opx2-definition-mode-map "t"     'fi:definition-mode-toggle-trace)
  (define-key fi:opx2-definition-mode-map "q"     'fi:definition-mode-quit)

  (use-local-map fi:opx2-definition-mode-map)
  
  (run-hooks 'fi:definition-mode-hook))
