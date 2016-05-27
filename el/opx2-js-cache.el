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
(defvar *ojs-required-fixes* '(("sc8567" "3.41")
			       ))

(defvar-resetable *ojs-configuration-status* nil 'ojs-reset)

;; check that we have the proper configuration
(defun ojs-configuration-ok ()
  (unless *ojs-configuration-status*
    (setq *ojs-configuration-status*
	  (if (check-fixes-configuration *ojs-required-fixes*)
	      :ok :ko)))
  (cond ((and (eq *ojs-configuration-status* :ok)
	      (fi::lep-open-connection-p))
	 t)
	(t
	 nil)))

;;;;; global cache for function and methods definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *js-variable-name*
  "[_a-zA-Z][_a-zA-Z0-9]*")

(defconst *js-function-name*
  *js-variable-name*)

(defconst *js-class-name*
  *js-variable-name*)

(defconst *js-namespace-name*
  *js-variable-name*)

(defconst *js-type*
  (format "\\(?:%s\\.\\)?%s" *js-namespace-name* *js-variable-name*))

(defconst *js-function-name-with-namespace*
  (format "%s\\.%s" *js-namespace-name* *js-function-name*))

(defconst *ojs-function-method-definition-regexp* 
  (format "^\\s-*\\(?:function\\|method\\)\\s-+\\(%s\\).*$" *js-function-name*))

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar-resetable *ojs-buffers-functions-cache* nil 'ojs-save)
;; contains the master regexp for syntax highlighting
(defvar-resetable *ojs-buffers-functions-cache-regexp* nil 'ojs-save)

(defun ojs-functions-in-buffers ()
  (or *ojs-buffers-functions-cache*
      (progn (generate-ojs-functions-cache)
	     *ojs-buffers-functions-cache*)))

(defun ojs-functions-in-buffers-regexp ()
  (or *ojs-buffers-functions-cache-regexp*
      (progn (generate-ojs-functions-cache)
	     *ojs-buffers-functions-cache-regexp*)))

(defun generate-ojs-functions-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-functions-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-function-method-definition-regexp*))
  ;; regexp cache
  (setq *ojs-buffers-functions-cache-regexp*
	(format "\\(%s\\)(" (js--regexp-opt-symbol (loop for item in *ojs-buffers-functions-cache*
							 collect (car item))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lock-register function : we build a small cache with
;; all the variables in the functions defined from start to end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *js-local-vars-cache* nil)

(defconst *js-var-with-type-regexp* 
  (format "\\<var\\>\\s-+\\(\\<%s\\>\\)?\\s-*\\(\\<%s\\>\\)\\s-*\\(?:;\\|=.+\\|in.*\\)" *js-type* *js-variable-name*))

(defconst *js-function-method-header* 
  (format "\\<\\(?:function\\|method\\)\\>\\s-+\\(%s\\)\\(?:\\s-+on\\s-+\\(%s\\)\\)?\\s-*(" *js-function-name* *js-type*))

(defun build-local-vars-cache (start end)
  (let (res (pos 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(let ((boundaries (function-boundaries))
	      (vars-ht (make-hash-table :test 'equal))
	      vars-list)	      
	  (cond (boundaries
		 ;; we are in a function, search for vars
		 ;; first, get the arguments of the function
		 (let* ((start-function (goto-char (car boundaries)))
			(function-name (when (re-search-forward *js-function-method-header* (line-end-position) t)
					 ;; go back one char to be on the (, which the end of our regexp
					 (forward-char -1)
					 (match-string-no-properties 1)))
			(class-name    (match-string-no-properties 2))
			(begin-args (progn (while (and (not (fast-looking-at "("))
						       (< (point) (line-end-position)))
					     (forward-char))
					   (if (fast-looking-at "(") (point) nil)))
			(end-args (progn (while (and (not (fast-looking-at ")"))
						     (< (point) (line-end-position)))
					   (forward-char))
					 (if (fast-looking-at ")") (point) nil)))
			(begin-function (progn (while (and (not (fast-looking-at "{"))
							   (< (point) (line-end-position)))
						 (forward-char))
					       (if (fast-looking-at "{") (point) nil)))
			(end-function (or (cdr boundaries)
					  (point-max)))
			(function-line (when end-args (buffer-substring-no-properties start-function (1+ end-args) ))))		   
		   ;; function / methods arguments
		   (when (and function-line begin-args end-args)
		     (dolist (arg (split-string (buffer-substring-no-properties (1+ begin-args) end-args) "[ \t,]+" t))
		       ;; if the args are separated by spaces, we have a type arg
		       (let ((type-and-arg (split-string arg)))
			 (push (downcase (or (second type-and-arg)
					     (car type-and-arg))) vars-list)
			 (puthash (downcase (or (second type-and-arg)
						(car type-and-arg)))
				  (list function-line (car type-and-arg))
				  vars-ht)))
		     ;; var in the function
		     (when (and begin-function end-function)
		       (goto-char begin-function)
		       (while (re-search-forward *js-var-with-type-regexp* end-function t)
			 (push (downcase (match-string-no-properties 2)) vars-list)
			 (puthash (downcase (match-string-no-properties 2))
				  (list (match-string-no-properties 0)
					(match-string-no-properties 1))
				  vars-ht)))
		     ;; ;; JS2 : if in a method, method members
		     ;; (when (eq major-mode 'pjs-mode)
		     ;;   (let ((class (convert-pjs-type class-name)))
		     ;; 	 (when class
		     ;; 	   (maphash #'(lambda (k v)
		     ;; 			(push (downcase k) vars-list)
		     ;; 			(puthash (downcase k)
		     ;; 				 (list v "Class member")
		     ;; 				 vars-ht))
		     ;; 		    (pjs-class-members (car class) (cdr class))))))
		     (push (list :function function-name
				 :class class-name
				 :start begin-function
				 :end end-function
				 :vars vars-ht
				 :vars-regexp (js--regexp-opt-symbol vars-list)
				 :next (setq pos (1+ pos)))
			   res))
		   (goto-char (1+ end-function))))
		((goto-start-of-next-function end)
		 t)
		(t
		 (goto-char (1+ end)))))))
    (setq *js-local-vars-cache* (reverse res))))

;; returns the local function environment around point
(defun get-local-function-environment (&optional position)
  (if position
      (and (<= position (length *js-local-vars-cache*))
	   (nth position *js-local-vars-cache*))    
    (catch 'exit    
      (dolist (context *js-local-vars-cache*)
	(cond ((and (>= (point) (getf context :start))
		    (<= (point) (getf context :end)))
	       ;; we found the context, yay	       
	       (throw 'exit context))
	      ((< (point) (getf context :start))
	       ;; too late, we exit
	       (throw 'exit (getf context :start))))))))

;;;;; buffer dependant cache for script-level var definitions and global vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-file-vars-regexp*  
  "^var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")

;; (defconst *pjs-file-vars-regexp*
;;   "^var\\s-*\\(?:[[:word:].]+\\)?\\s-+\\(\\w+\\)\\s-*[=;].*$")
;;  "^var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")

(defconst *ojs-global-vars-regexp*  
  "^\\s-*global\\s-+var\\s-+\\(\\w+\\)\\s-*\\(=\\|in\\|;\\).*$")

;; used for autocomplete 
(defvar-resetable *ojs-buffers-vars-cache* nil 'ojs-save)
;; contains the master regexp for syntax highlighting
(defvar-resetable *ojs-buffers-vars-cache-regexp* nil 'ojs-save)

(defun ojs-vars-in-buffer ()
  (interactive)
  (or *ojs-buffers-vars-cache*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache*)))

(defun ojs-vars-in-buffer-regexp ()
  (or *ojs-buffers-vars-cache-regexp*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache-regexp*)))

(defun generate-ojs-vars-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-vars-cache* (append 
				  (ojs-find-candidates-from-regexp ;;(if (eq major-mode 'pjs-mode)
								   ;;    *pjs-file-vars-regexp*
								     *ojs-file-vars-regexp*)
				  (when (eq major-mode 'opx2-js-mode)
				    (ojs-find-candidates-from-regexp-in-buffers *ojs-global-vars-regexp*))))
  ;; regexp cache
  (setq *ojs-buffers-vars-cache-regexp*
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-vars-cache*
				     collect (car item))))
  )

;;;;; global cache for kernel function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; cache reset on file save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun js-reset-vars (type)
  (dolist (item (gethash type *js-vars-to-reset*))
    (setf (symbol-value item) nil)))


(defun ojs-reset-cache-on-save ()
  (js-reset-vars 'ojs-save))

;;;;; generic function to find candidates based on a regexp 
;;;;; it returns a candidates list containing cons (name . documentation) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ojs-find-candidates-from-regexp (regexp &optional already-matched)
  (let (candidate
	candidates
	(point (point))
	)
    (save-excursion
      ;; Search forward from the line after the current one
      (forward-line 1)
      (while  (re-real-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; search backward from the start of the current line
      (goto-char point)
      (beginning-of-line)
      (while  (re-real-search-backward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; Search forward
      (nreverse candidates))))

(defun ojs-find-candidates-from-regexp-in-buffers (regexp )
  (let (candidates)
    (dolist (buffer (buffer-list))
      (when (or (derived-mode-p (buffer-local-value 'major-mode buffer))
		(equal (buffer-local-value 'major-mode buffer) 'opx2-js-mode))
	(with-current-buffer buffer
	  (let ((found (ojs-find-candidates-from-regexp regexp candidates)))
	    (setq candidates (append candidates found))))))
    candidates))

