;;; semantic configuration for pjs files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'semantic)

(defconst *semantic-pjs-tag-list-attributes*
  '( :members
     :attributes
     :arguments
     :children
     :local-vars))

(defvar-local *semantic-parse-cache* nil)

;; we cache the last result for each region
(defun semantic-pjs-parse-region (start end &optional nonterminal depth returnonerror)
  (unless *semantic-parse-cache*
    (setq *semantic-parse-cache* (make-hash-table :test 'eq)))
  (when (fi::lep-open-connection-p)
    (let ((tags (fi:eval-in-lisp "(jvs::semantics-generate-tags %S)" (buffer-substring-no-properties start end)))
;;	  (tags (fi:eval-in-lisp "(when (fboundp 'jvs::semantics-generate-tags-for-file) (jvs::semantics-generate-tags-for-file %S))" (buffer-file-name)))
	  res)
      (message "Parsing %s to %s with %s : %s tags found" start end nonterminal (length tags))
      (cond (tags
	     ;; cook the tags	
	     (dolist (tag tags)
	       (when (and tag
			  (= (length tag) 7))
		 (push (semantic-pjs-expand-tag tag) res)))
	     (puthash (intern (format "%s.%s" start end))
		      (reverse res)
		      *semantic-parse-cache*))
	    (t
	     (gethash (intern (format "%s.%s" start end)) *semantic-parse-cache*))))))

(defun semantic-pjs-expand-tag (tag)
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
	  (push (mapcar 'semantic-pjs-expand-tag v) new-list)
	(push v new-list))) 	
    (setf (third tag) (reverse new-list))
    (car (semantic--tag-expand tag))))

(defun semantic-tag-local-vars (tag)
  (semantic-tag-get-attribute tag :local-vars))

(defun semantic-tag-children (tag)
  (semantic-tag-get-attribute tag :children))

(defun pjs-semantic-tag-components (tag)
  ;; returns the children of the tags
  (let (res
	(attr-list (third tag)))
    (dolist (member attr-list)
      (when (memq (car member) *semantic-pjs-tag-list-attributes*)
	(push res (second member))))
    res))

(defun pjs-semantic-tag-components (tag)
  ;; iterate through the members
  (let (new-list
	(attr-list (third tag)))        
    (do* ((list attr-list (cddr list))
	  (k (car list) (car list))
	  (v (second list) (second list))
	  )
	((null list))
      (when (memq k *semantic-pjs-tag-list-attributes*)
	(setq new-list (append new-list v))))
    new-list))

;; recursively collect tags
;; if not inside a "block" (block / function ) tag, take the parent
(defun semantic-collect-local-vars-from-tag (tag point)
  (let (var-list)
    (cond ((memq (semantic-tag-class tag) '(block function))
	   (catch 'exit
	     ;; collect arguments
	     (dolist (arg (semantic-tag-function-arguments tag))
	       (push arg var-list))
	     ;; iterate on local vars of the tags
	     (dolist (var (semantic-tag-local-vars tag))
	       ;;	(when (> point (semantic-tag-start var))
	       (push var var-list))
	     ;;)
	     ;; take the local variables of the parent
	     (let ((parent (semantic-find-tag-parent-by-overlay tag)))
	       (unless (eq parent tag)
		 (append var-list (semantic-collect-local-vars-from-tag (semantic-find-tag-parent-by-overlay tag) point))))))
	  ((semantic-tag-p tag)
	   (semantic-collect-local-vars-from-tag (semantic-current-tag-parent) (point)))
	  (t
	   nil))))
	  

(defun semantic-pjs-get-local-variables (&optional point)
  "Get the local variables based on POINT's context.
Local variables are returned in Semantic tag format.
This can be overridden with `get-local-variables'."
  (let ((tag (car (last (semantic-find-tag-by-overlay point)))))
    ;; only one tag for now
    (semantic-collect-local-vars-from-tag tag (or point (point)))))
  
;;;###autoload
(defun semantic-default-pjs-setup ()
  "Setup hook function for pjs files and Semantic."
  (semantic-install-function-overrides
   '((parse-region . semantic-pjs-parse-region)
     (get-local-variables . semantic-pjs-get-local-variables)
     (tag-components . pjs-semantic-tag-components)
     ))
  (setq semantic-parser-name "PJS"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
	)
  )

(add-hook 'pjs-mode-hook 'semantic-default-pjs-setup)

(provide 'pjs-semantic)

;;; display debug 
(defun fi::lep-connection-filter (process string)
  ;; When a complete sexpression comes back from the lisp, read it and then
  ;; handle it
  (when fi::debug-subprocess-filter
    (push string fi::debug-subprocess-filter-output))
  (let ((inhibit-quit t)
	(buffer (or (process-buffer process)
		    (get-buffer-create " LEP temp "))))
    (when fi::trace-lep-filter
      (fi::trace-debug (format "lep-connection-filter: %s" string)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (insert string))
    (let (form error)
      (while	  
	  (condition-case nil
	      (with-current-buffer buffer
;;		(set-buffer buffer)
		(and
		 (null error)
		 (not (eq (point-max) (point-min)))
		 (progn
		   (goto-char (point-min))
		   (forward-sexp)
		   (let ((p (point)))
		     (goto-char (point-min))
		     (condition-case e
			 (progn (setq form (read (current-buffer)))				
				t)
		       (error
			(message "Error in read : %s" e)
			(setq error t)))
		     (delete-region (point-min) p))
		   t)))
	    (error nil))
	(if error
	    (error "error reading return value: %s" string)
	  (fi::handle-lep-input process form))))))


;; speedbar

(defun pjs-fetch-dynamic-tags (file)
  )

;;(semantic-sb-insert-tag-table ("Variables" ("*semantic-pjs-tag-list-attributes*" variable (:constant-flag t :default-value (quote (:members :attributes :arguments :children :local-vars))) nil #<overlay from 108 to 233 in pjs-semantic.el>)) ("Defuns" ("fi::lep-connection-filter" function (:arguments ("process" "string")) nil #<overlay from 3012 to 4170 in pjs-semantic.el>) ("semantic-default-pjs-setup" function nil nil #<overlay from 2535 to 2911 in pjs-semantic.el>) ("semantic-pjs-get-local-variables" function (:arguments ("point")) nil #<overlay from 2145 to 2516 in pjs-semantic.el>) ("semantic-collect-local-vars-from-tag" function (:arguments ("tag" "point")) nil #<overlay from 1506 to 2143 in pjs-semantic.el>) ("semantic-tag-children" function (:arguments ("tag")) nil #<overlay from 1396 to 1476 in pjs-semantic.el>) ("semantic-tag-local-vars" function (:arguments ("tag")) nil #<overlay from 1310 to 1394 in pjs-semantic.el>) ("semantic-pjs-expand-tag" function (:arguments ("tag")) nil #<overlay from 829 to 1308 in pjs-semantic.el>) ("semantic-pjs-parse-region" function (:arguments ("start" "end" "nonterminal" "depth" "returnonerror")) nil #<overlay from 235 to 827 in pjs-semantic.el>)) ("Requires" ("semantic" include nil nil #<overlay from 87 to 106 in pjs-semantic.el>)) ("Provides" ("semantic-pjs" package nil nil #<overlay from 2968 to 2991 in pjs-semantic.el>)) ("Misc"))
