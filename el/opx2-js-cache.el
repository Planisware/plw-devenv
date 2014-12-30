;;;;; global cache for function and methods definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-function-method-definition-regexp* 
  "^[ \t]*\\(?:function\\|method\\)[ \t]+\\(\\w+\\).*$")

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar *ojs-buffers-functions-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *ojs-buffers-functions-cache-regexp* nil)

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
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-functions-cache*
				     collect (car item)))))

;;;;; global cache for global var definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-global-vars-regexp*  
  "^[ \t]*global[ \t]+var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")

;; contains list of cons (name . documentation)
;; used for autocomplete 
(defvar *ojs-buffers-global-vars-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar *ojs-buffers-global-vars-regexp* nil)

(defun ojs-global-vars-in-buffers ()
  (or *ojs-buffers-global-vars-cache*
      (progn (generate-ojs-global-vars-cache)
	     *ojs-buffers-global-vars-cache*)))

(defun ojs-global-vars-in-buffers-regexp ()
  (or *ojs-buffers-global-vars-regexp*
      (progn (generate-ojs-global-vars-cache)
	     *ojs-buffers-global-vars-regexp*)))

(defun generate-ojs-global-vars-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-global-vars-cache* (ojs-find-candidates-from-regexp-in-buffers *ojs-global-vars-regexp*))
  ;; regexp cache
  (setq *ojs-buffers-global-vars-regexp*
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-global-vars-cache*
				     collect (car item)))))

;;;;; buffer dependant cache for global var definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ojs-vars-regexp*  
  "^var[ \t]+\\(\\w+\\)[ \t]*\\(=\\|in\\|;\\).*$")

;; used for autocomplete 
(defvar-local *ojs-buffers-vars-cache* nil)
;; contains the master regexp for syntax highlighting
(defvar-local *ojs-buffers-vars-regexp* nil)

(defun ojs-global-vars-in-buffers ()
  (or *ojs-buffers-vars-cache*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-cache*)))

(defun ojs-global-vars-in-buffers-regexp ()
  (or *ojs-buffers-global-vars-regexp*
      (progn (generate-ojs-vars-cache)
	     *ojs-buffers-vars-regexp*)))

(defun generate-ojs-vars-cache ()
  ;; generate the documentation cache
  (setq *ojs-buffers-global-vars-cache* (ojs-find-candidates-from-regexp *ojs-vars-regexp*))
  ;; regexp cache
  (setq *ojs-buffers-global-vars-regexp*
	(js--regexp-opt-symbol (loop for item in *ojs-buffers-global-vars-cache*
				     collect (car item)))))

;;;;; global cache for kernel function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; cache reset on file save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ojs-reset-cache ()
  (setq *ojs-buffers-functions-cache* nil
	*ojs-buffers-functions-cache-regexp* nil
	*ojs-buffers-global-vars-cache* nil
	*ojs-buffers-global-vars-regexp* nil
	*ojs-buffers-vars-cache* nil
	*ojs-buffers-vars-regexp* nil))

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
      (while  (re-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 1))
	(unless (or (member candidate candidates) (and already-matched (listp already-matched) (member candidate already-matched)))
	  (push (cons candidate (match-string-no-properties 0)) candidates)
	  ))
      ;; search backward from the start of the current line
      (goto-char point)
      (beginning-of-line)
      (while  (re-search-backward regexp nil t)
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
