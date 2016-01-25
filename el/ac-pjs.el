(require 'auto-complete)

;;;;; configuration of the hook

;;; find functions and variables from current namespaces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "pjs-namespaces-functions"
  '((prefix . pjs-namespaces-functions-ac-prefix)
    (candidates . pjs-namespaces-functions-ac-candidates)
;;    (document . pjs-namespaces-functions-ac-document)
;;    (action . pjs-namespaces-functions-ac-action)
    (symbol . "f ")
    (requires . -1)))

(defvar *current-pjs-namespace-prefix* nil)

(defun pjs-namespaces-functions-ac-prefix ()
  ;; detects the current namespace
  (save-match-data
    (let ((match (re-search-backward (format "\\(\\<%s\\>\\)\\(\\.\\)\\(\\<%s\\>\\)?\\=" *js-variable-name* *js-variable-name*) (line-beginning-position) t)))
      (when match
	(setq *current-pjs-namespace-prefix* (match-string-no-properties 1))
	(1+ (match-beginning 2))))))

(defun pjs-namespaces-functions-ac-action ()  
  )

(defun pjs-namespaces-functions-ac-candidates ()
  (let (res
	(functions (list-pjs-namespace-functions *current-pjs-namespace-prefix*))
	(vars      (list-pjs-namespace-variables *current-pjs-namespace-prefix*)))
    (append functions vars res)))

(defun pjs-namespaces-functions-ac-document (docstr)
  docstr)

;;; find locally defined variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pjs-var-documenation (tag)
  (let ((str (format "Variable : %s" (semantic-tag-name tag))))
    (when (semantic-tag-get-attribute tag :type)
      (setq str (concat str (format " Type : %s"  (semantic-tag-get-attribute tag :type)))))
    (when (semantic-tag-get-attribute tag :default-value)
      (setq str (concat str (format " Default value : %s"  (semantic-tag-get-attribute tag :default-value)))))
    str))

(ac-define-source "pjs-local-vars"
  '((candidates . pjs-local-vars-candidates)
    (document . pjs-local-vars-documentation)
    (action . pjs-local-vars-action)
    (symbol . "v ")
    (requires . -1)))

(defun pjs-local-vars-candidates ()
  (let ((vars (semantic-get-local-variables))
	res)
    (dolist (var vars)
      (when (> (point) (semantic-tag-end var))
	(push (cons (semantic-tag-name var)
		    (pjs-var-documenation var))
	      res)))
    res))

(defun candidate-documentation (candidate)
  (get-pos-property 0 'value candidate))

(defvar *my-candidate* nil)

(defun pjs-local-vars-action ()
  (message (candidate-documentation candidate)))

(defun pjs-local-vars-documentation (docstr)
  docstr)

;; definition of a new autocomplete mode
(defun pjs-setup-auto-complete-mode ()
  "Setup ac-js2 to be used with auto-complete-mode."
;;  (message "Setting autocomplete")
  ;; js functions defined in the file
  (setq ac-sources nil)
  ;;(setq ac-sources '(ac-source-ojs-functions ac-source-ojs-methods ac-source-ojs-vars ac-source-ojs-kernel))
  
  (add-to-list 'ac-sources 'ac-source-pjs-namespaces-functions)
  (add-to-list 'ac-sources 'ac-source-pjs-local-vars)
 
  ;; (add-to-list 'ac-sources 'ac-source-ojs-vars)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-kernel)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-dictionary)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-members)
  ;; (add-to-list 'ac-sources 'ac-source-ojs-classes)

  ;; initialize kernel completion
;;  (ojs-kernel-ac-init)

  ;; classes completion
;;  (ojs-classes-ac-init)

  ;; dictionary cache
;;  (setq *ac-dictionary-candidates-cache* (ac-file-dictionary (concat *opx2-network-folder-work-path* "devenv/el/dict/opx2-js-mode")))
  
  ;; display doc quickly
  (setq ac-quick-help-delay 0.1)
  ;; autocomplete only when I ask for it
  (setq ac-auto-start nil)
)

(add-hook 'pjs-mode-hook 'pjs-setup-auto-complete-mode)
