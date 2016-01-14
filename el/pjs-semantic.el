;;; semantic configuration for pjs files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'semantic)

(defconst *semantic-pjs-tag-list-attributes*
  '( :members
     :attributes
     :arguments
     :children
     :local-vars))

(defun semantic-pjs-parse-region (start end &optional nonterminal depth returnonerror)
  (when (fi::lep-open-connection-p)
    (message "Parsing %s %s" start end)
    (let (;;(tags (fi:eval-in-lisp "(jvs::semantics-generate-tags %S)" (buffer-substring-no-properties start end)))
	  (tags (fi:eval-in-lisp "(when (fboundp 'jvs::semantics-generate-tags-for-file) (jvs::semantics-generate-tags-for-file %S))" (buffer-file-name)))
	  res)
      (message "tags are " tags)
      (when tags
	;; cook the tags	
	(dolist (tag tags)
	  (push (semantic-pjs-expand-tag tag) res)))
      (reverse res))))

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
    (setf (third tag) (reverse new-list)))
  (car (semantic--tag-expand tag)))

(defun semantic-tag-local-vars (tag)
  (semantic-tag-get-attribute tag :local-vars))

(defun semantic-tag-children (tag)
  (semantic-tag-get-attribute tag :children))

;; recursively collect tags
(defun semantic-collect-local-vars-from-tag (tag point)
  (let (var-list)
    (catch 'exit
      ;; collect arguments
      (dolist (arg (semantic-tag-function-arguments tag))
	(push arg var-list))
      ;; iterate on local vars of the tags
      (dolist (var (semantic-tag-local-vars tag))
	(when (> point (semantic-tag-start var))
	  (push var var-list)))
      ;; go in the proper child
      (dolist (child (semantic-tag-children tag))
	(when (and (>= point (semantic-tag-start child))
		   (<= point (semantic-tag-end child)))
	  (throw 'exit (append var-list (semantic-collect-local-vars-from-tag child point)))))
      var-list)))

(defun semantic-pjs-get-local-variables (&optional point)
  "Get the local variables based on POINT's context.
Local variables are returned in Semantic tag format.
This can be overridden with `get-local-variables'."
  (let ((tags (semantic-find-tag-by-overlay point)))
    ;; only one tag for now
    (semantic-collect-local-vars-from-tag (car tags) (or point (point)))))
  
;;;###autoload
(defun semantic-default-pjs-setup ()
  "Setup hook function for pjs files and Semantic."
  (semantic-install-function-overrides
   '((parse-region . semantic-pjs-parse-region)
     (get-local-variables . semantic-pjs-get-local-variables)
     ))
  (setq semantic-parser-name "PJS"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
	)
  )

;;(add-hook 'pjs-mode-hook 'semantic-default-pjs-setup)

(provide 'semantic-pjs)


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
