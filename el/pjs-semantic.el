;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT


(require 'semantic)
;;(require 'semantic/edit)

;;; replace prefix in semantic shortcut , -> $

(defun redefine-semantic-shortcuts ()
  (cl-labels ((map-keymap (map)
			  (dolist (item (cdr map))
			    (when (eq (car item) 44)
			      (setcar item 36))
			    (when (keymapp (cdr item))
			      (map-keymap (cdr item))))))
    (map-keymap semantic-mode-map)))

(add-hook 'semantic-mode-hook 'redefine-semantic-shortcuts)

(semantic-mode 1)

(defconst *semantic-pjs-tag-list-attributes*
  '( :members
     :attributes
     :arguments
     :children
     :local-vars))

(defun pjs-semantic-tag-block-p (tag)
  (memq (semantic-tag-class tag) '(block function type)))

(defun pjs-semantic-tag-var-p (tag)
  (eq (semantic-tag-class tag) 'variable))

;; same as semantic-find-tag-by-overlay but returns only "block" tags (block / functions / classes)
(defun pjs-semantic-find-block-tag-by-overlay (&optional positionormarker buffer)
  (let (res)
    (dolist (tag (semantic-find-tag-by-overlay positionormarker buffer))
      (when (pjs-semantic-tag-block-p tag)
	(push tag res)))
    (reverse res)))		 

(setq semantic-edits-verbose-flag nil)

;;(defconst *pjs-start-block-regexp* (format "^\\s-*\\(\\(function\\|method\\)\\s-+\\<%s\\>\\s-*(.*)\\|class\\s-+\\<%s\\>\\)\\s-*{" *js-function-name* *js-function-name*))
;; try the "lazy" version first
(defconst *pjs-start-block-regexp*
  (format "^\\s-*\\(%s\\|method\\|class\\).*{" *pjs-function-keyword*))

(defvar *pjs-parse-changes* nil)

(defun pjs-semantic-parse-changes ()
  "Incrementally reparse the current buffer.
Incremental parser allows semantic to only reparse those sections of
the buffer that have changed.  This function depends on
`semantic-edits-change-function-handle-changes' setting up change
overlays in the current buffer.  Those overlays are analyzed against
the semantic cache to see what needs to be changed."
  (let ((*pjs-parse-changes* t)
	(changed-tags
         ;; Don't use `semantic-safe' here to explicitly catch errors
         ;; and reset the parse tree.
         (catch 'semantic-parse-changes-failed
	   (semantic-edits-incremental-parser-1)
           ;; (if debug-on-error
	   ;;     ;;               (pjs-semantic-edits-incremental-parser-1)
	   ;;     (semantic-edits-incremental-parser-1)
           ;;   (condition-case err
	   ;; 	 ;; (pjs-semantic-edits-incremental-parser-1)
	   ;; 	 (semantic-edits-incremental-parser-1)
           ;;     (error
           ;;      (message "incremental parser error: %S"
	   ;; 		 (error-message-string err))
           ;;      t))))))
	   )))
    (when (eq changed-tags t)
      ;; Force a full reparse.
      (semantic-edits-incremental-fail)
      (setq changed-tags nil))
    (when (eq changed-tags :error)
      (message "Error in parsial parsing, setting up to date")
      (semantic-parse-tree-set-up-to-date))
    changed-tags))

;; parse the buffer block by block
(defun pjs-semantic-parse-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let (list
	  (cur-point (point)))      
      ;;      (condition-case err
      (while (< (point) (point-max))
	(let ((next-function (re-real-search-forward *pjs-start-block-regexp* nil t)))
	  ;;(let ((next-function (re-search-forward *pjs-start-block-regexp* nil t)))
	  (if next-function
	      (let* ((start-inter cur-point)
		     (end-inter (1- (line-beginning-position)))
		     (start-block (line-beginning-position))
		     (end-block (progn (backward-char) (forward-sexp)(point))))
		(setq cur-point (point))
		(push (list start-inter end-inter) list)
		(push (list start-block end-block) list))
	    (progn (push (list (point) (point-max)) list)
		   (goto-char (point-max))))))
      (pjs-semantic-parse-list (reverse list)))))

(defun pjs-semantic-parse-region (start end &optional nonterminal depth returnonerror)
  (prog1
      (if (and (eq start (point-min))
	       (eq end (point-max)))
	  (pjs-semantic-parse-buffer)
	(pjs-semantic-parse-list (list (list start end)) nonterminal depth returnonerror))
    ;; refresh syntax highlighting
    (jit-lock-refontify start end)))

(defface pjs-semantic-error-font
  '((t 
     ;;       :underline (:color "red" :style wave)))
     :background "grey25"))
  "Highlight block of error "
  )

(defvar pjs-semantic-error-font
  'pjs-semantic-error-font)

(defface pjs-semantic-error-highlight
  '((t 
     ;;       :background "#255199206"))
     :background "red4"))
  "Highlight speficic error"
  )

(defvar pjs-semantic-error-highlight
  'pjs-semantic-error-highlight)

(defvar *pjs-verbose-parsing* nil)

(defun pjs-semantic-parse-list (list &optional nonterminal depth returnonerror)
  ;; list is a list of start / end coordinates in the buffer
  (when (pjs-configuration-ok)
    (let ((context 10)
	  (ltags (fi:eval-in-lisp "(jvs::semantics-generate-tags-for-list '%S)"
				  (mapcar #'(lambda (l)
					      (buffer-substring-no-properties (car l) (second l)))
					  list)))
	  res
	  (i 0))
      (when ltags
	(do* ((i 0 (+ 1 i))
	      (tags (nth i ltags) (nth i ltags))
	      (coords (nth i list) (nth i list))
	      (start (car coords) (car coords))
	      (end (second coords) (second coords)))
	    ((= i (length list)))	 	  
	  
	  (when *pjs-verbose-parsing*
	    (let ((parsed-block (cond ((<= (- end start) context)
				       (buffer-substring-no-properties start end))
				      (t
				       (format "[%s||%s]"
					       (buffer-substring-no-properties start (min (point-max) (+ context start)))
					       (buffer-substring-no-properties (max (point-min) (- end context)) end))))))
	      (message "Parsing %s [%s:%s] [%s] with %s : %s tags found"
		       *pjs-parse-changes*
		       start
		       end
		       parsed-block
		       nonterminal
		       (if (listp tags) (length tags) tags))))
	  
	  ;; remove previous error overlays
	  (dolist (overlay (overlays-in start end))
	    (when (semantic-overlay-get overlay 'pjs-error)
	      (delete-overlay overlay)))
	  
	  (when (stringp tags)
	    ;; we have an error !!
	    ;; try to highlight the error       
	    (when (string-match "At line\\s-*:\\s-*\\([0-9]+\\)\\s-*,\\s-*character\\s-*:\\s-*\\([0-9]+\\)\\s-*:" tags)
	      (let ((line (string-to-number (match-string-no-properties 1 tags)))
		    (char (string-to-number (match-string-no-properties 2 tags))))
		(save-excursion
		  (let ((specific-overlay (make-overlay (progn (goto-char start) (next-line (1- line)) (beginning-of-line)(forward-char char) (point))
							(line-end-position))))
		    (overlay-put specific-overlay
				 'face pjs-semantic-error-highlight)
		    (overlay-put specific-overlay
				 'pjs-error t)
		    (overlay-put specific-overlay
				 'help-echo tags)))))
	    ;; otherwise create an error overlay for the whole block
	    (let ((error-overlay (make-overlay start end)))
	      (overlay-put error-overlay
			   'face pjs-semantic-error-font)
	      (overlay-put error-overlay
			   'pjs-error t)
	      (overlay-put error-overlay
			   'help-echo tags)))
	  
	  (cond ((and (stringp tags) *pjs-parse-changes*)
		 (throw 'semantic-parse-changes-failed :error))
		((listp tags)
		 ;; cook the tags	
		 (dolist (tag tags)
		   (when (and tag
			      (= (length tag) 7))
		     (push (pjs-semantic-expand-tag tag start) res))))))
	(reverse res)))))

(defun pjs-semantic-expand-tag (tag start)
  ;; iterate through the members
  (let (new-list
	(attr-list (third tag)))        
    (do* ((list attr-list (cddr list))
	  (k (car list) (car list))
	  (v (second list) (second list))
	  )
	((null list))
      (push k new-list)
      (if (memq k *semantic-pjs-tag-list-attributes*)
	  (push (mapcar #'(lambda (tag) (when tag (pjs-semantic-expand-tag tag start))) v) new-list)
	(push v new-list))) 	
    (setf (third tag) (reverse new-list))
    ;; lisp positions are off by one
    (setf (sixth tag) (+ (sixth tag) start))
    (setf (seventh tag) (+ (seventh tag) start))
    ;; TODO : (semantic--tag-put-property (car l) 'reparse-symbol $nterm)
    ;; use the reparse symbol ??
    ;; for now, only reparse blocks, functions and classes. Not the variables (too random).
    (unless (pjs-semantic-tag-block-p tag)
      (semantic--tag-put-property tag 'no-single-reparse t))
    (car (semantic--tag-expand tag))))

(defun pjs-semantic-tag-local-vars (tag)
  (mapcan #'(lambda (c) (when (pjs-semantic-tag-var-p c) (list c))) (pjs-semantic-tag-children tag)))

(defun pjs-semantic-tag-children (tag)
  (semantic-tag-get-attribute tag :children))

;; depends of the type
;; function : -> children
;; block    : -> children
;; class    : -> members

(defun pjs-semantic-tag-components (tag)
  ;; returns the children of the tags
  (cond ((memq (semantic-tag-class tag) '(function block))
	 (pjs-semantic-tag-children tag))
	((eq (semantic-tag-class tag) 'type)
	 (semantic-tag-get-attribute tag :members))))

;; recursively collect tags
;; if not inside a "block" (block / function ) tag, take the parent
(defun pjs-semantic-collect-local-vars-from-tag (tag &optional seen)
  (let (var-list)    
    (cond ((null tag)
	   nil)
	  ((semantic-tag-p tag)
	   ;;(memq (semantic-tag-class tag) '(block function))
	   ;; iterate on local vars of the tags
	   (dolist (var (pjs-semantic-tag-local-vars tag))
	     (push var var-list))

	   ;; take the local variables of the parent
	   (let ((parent (semantic-find-tag-parent-by-overlay tag)))
	     (unless (or (eq tag parent)
			 (memq parent seen))
;;	       (message (format "tag %s vars %s" (semantic-find-tag-parent-by-overlay tag) (pjs-semantic-collect-local-vars-from-tag (semantic-find-tag-parent-by-overlay tag) (concatenate 'list (list tag) seen))))
	       (append var-list (pjs-semantic-collect-local-vars-from-tag (semantic-find-tag-parent-by-overlay tag) (concatenate 'list (list tag) seen))))))
	  ;; ((semantic-tag-p tag)
	  ;;  (let ((parent (semantic-current-tag-parent)))
	  ;;    (unless (or (eq tag parent)
	  ;; 		 (memq parent seen))
	  ;;      (pjs-semantic-collect-local-vars-from-tag parent (concatenate 'list (list tag) seen)))))
	  (t
	   nil))))


(defun pjs-semantic-get-local-variables (&optional point)
  "Get the local variables based on POINT's context.
Local variables are returned in Semantic tag format.
This can be overridden with `get-local-variables'."
  (let ((tag (car (last (semantic-find-tag-by-overlay (or point (point)))))))
    ;; only one tag for now
    (pjs-semantic-collect-local-vars-from-tag tag)))

;;;###autoload
(defun semantic-default-pjs-setup ()
  "Setup hook function for pjs files and Semantic."
  (semantic-install-function-overrides
   '((parse-region . pjs-semantic-parse-region)
     (get-local-variables . pjs-semantic-get-local-variables)
     (tag-components . pjs-semantic-tag-components)
     (parse-changes . pjs-semantic-parse-changes)
     ))
  (setq semantic-parser-name "PJS"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
	)
  (semantic-force-refresh))

(add-hook 'pjs-mode-hook 'semantic-default-pjs-setup)

;; don't link to a buffer not from major mode
(defun semantic--tag-link-to-buffer (tag)
  "Convert TAG from using an overlay proxy to using an overlay.
This function is for internal use only."
  ;;  (unless (derived-mode-p 'prog-mode)
  ;;    (message "!!!!!!!! tag is %s buffer is %s" tag (current-buffer))
  ;;    (backtrace))
  (when (semantic-tag-p tag)
    (let ((o (semantic-tag-overlay tag)))
      (when (and (vectorp o) (= (length o) 2))
        (setq o (semantic-make-overlay (aref o 0) (aref o 1)
                                       (current-buffer)))
        (semantic--tag-set-overlay tag o)
        (semantic-overlay-put o 'semantic tag)
        ;; Clear the :filename property
        (semantic--tag-put-property tag :filename nil))
      ;; Look for a link hook on TAG.
      (semantic--tag-run-hooks tag 'link-hook)
      ;; Fix the sub-tags which contain overlays.
      (semantic--tag-link-list-to-buffer
       (semantic-tag-components-with-overlays tag)))))

(provide 'pjs-semantic)

;; patched from c:/Program Files (x86)/GNU Emacs 24.5/share/emacs/24.5/lisp/cedet/semantic/edit.el
;; do not return tags we don't want to parse on their own
(defun semantic-edits-change-leaf-tag (change)
  "A leaf tag which completely encompasses CHANGE.
If change overlaps a tag, but is not encompassed in it, return nil.
Use `semantic-edits-change-overlap-leaf-tag'.
If CHANGE is completely encompassed in a tag, but overlaps sub-tags,
return nil."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tags (nreverse
		(semantic-find-tag-by-overlay-in-region
		 start end))))
    
    (while (and tags
		(semantic--tag-get-property (car tags) 'no-single-reparse))
      (setq tags (cdr tags)))
    
    ;; A leaf is always first in this list
    (if (and tags
	     (<= (semantic-tag-start (car tags)) start)
	     (> (semantic-tag-end (car tags)) end))
	;; Ok, we have a match.  If this tag has children,
	;; we have to do more tests.
	(let ((chil (semantic-tag-components (car tags))))
	  (if (not chil)
	      ;; Simple leaf.
	      (car tags)
	    ;; For this type, we say that we encompass it if the
	    ;; change occurs outside the range of the children.
	    (if (or (not (semantic-tag-with-position-p (car chil)))
		    (> start (semantic-tag-end (nth (1- (length chil)) chil)))
		    (< end (semantic-tag-start (car chil))))
		;; We have modifications to the definition of this parent
		;; so we have to reparse the whole thing.
		(car tags)
	      ;; We actually modified an area between some children.
	      ;; This means we should return nil, as that case is
	      ;; calculated by someone else.
	      nil)))
      nil)))
