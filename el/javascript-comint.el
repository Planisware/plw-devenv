;;; DOC OPX2 javascript comint 

;; function to escape strings
(defun escape-quotes (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

;; send the javascript string to jvs::evaluate-javascript-string-in-string (defined in sc8567.lisp)
(defun opx2-js-comint-input-sender (proc input)
  ;(comint-output-filter proc (format "#%s\n" input))
  (let ((js-lisp (concat "(jvs::evaluate-javascript-string-in-string \"" (escape-quotes input) "\" nil)")))
;    (comint-output-filter proc (format "%s\n" js-lisp))
    (comint-output-filter proc (format "%s\n" (fi:eval-in-lisp js-lisp)))
    (comint-output-filter proc opx2-js-comint-prompt)
    ))

;; syntax highlighting
(setq *js-keywords*
 '(("Warning : .*" . font-lock-warning-face)
   ("Error   : .*" . font-lock-variable-name-face)
   ("Return  : .*" . font-lock-type-face)
   )
)

(defconst opx2-js-comint-prompt "OJS>> ")

;; we define a new mode
(define-derived-mode
  opx2-js-comint-mode comint-mode "OPX2 javascript evaluator"
  "Run an OPX2 javascript shell."
  
  (setq comint-prompt-regexp (concat "^" (regexp-quote opx2-js-comint-prompt)))
  (setq comint-input-sender 'opx2-js-comint-input-sender)
  (setq font-lock-defaults '(*js-keywords*))
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (let ((fake-proc
	   (condition-case nil
	       (start-process "ijsm" (current-buffer) "hexl")
	     (file-error (start-process "ijsm" (current-buffer) "cat")))))
      (set-process-query-on-exit-flag fake-proc nil)
      ;; Add a silly header
      (insert "OPX2 Javascript Evaluator\n")
      (set-marker
       (process-mark fake-proc) (point))
      (comint-output-filter fake-proc opx2-js-comint-prompt))))

(defun switch-to-script-comint ()
  (interactive)
  (let* ((buffer-name "*javascript-comint*")
	 (buffer (or (get-buffer buffer-name)
		     (get-buffer-create buffer-name)))
	 )
    (switch-to-buffer buffer-name)
    (opx2-js-comint-mode)
    ))

(when (eq *javascript-evaluator-mode* :comint)
  (global-set-key [f3] 'switch-to-script-comint))
