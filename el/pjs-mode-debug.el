;; -*- coding: windows-1252 -*- 
;; COPYRIGHT (C) PLANISWARE 2017
;; Distributed under the MIT License
;; See accompanying file LICENSE file or copy at http://opensource.org/licenses/MIT

(defvar *pjs-loudly-syntax-highlighting* t)

(defun pjs-font-lock-default-fontify-region (beg end loudly)
  "Fontify the text between BEG and END.
If LOUDLY is non-nil, print status messages while fontifying.
This function is the default `font-lock-fontify-region-function'."
  (setq loudly *pjs-loudly-syntax-highlighting*)
  ;;  (save-buffer-state
  ;; Use the fontification syntax table, if any.
  (with-syntax-table (or font-lock-syntax-table (syntax-table))
    (save-restriction
      (unless font-lock-dont-widen (widen))
      ;; Extend the region to fontify so that it starts and ends at
      ;; safe places.
      (let ((funs font-lock-extend-region-functions)
	    (font-lock-beg beg)
	    (font-lock-end end))
	(while funs
	  (setq funs (if (or (not (funcall (car funs)))
			     (eq funs font-lock-extend-region-functions))
			 (cdr funs)
		       ;; If there's been a change, we should go through
		       ;; the list again since this new position may
		       ;; warrant a different answer from one of the fun
		       ;; we've already seen.
		       font-lock-extend-region-functions)))
	(setq beg font-lock-beg end font-lock-end))
      ;; Now do the fontification.
      (font-lock-unfontify-region beg end)
      (when (and font-lock-syntactic-keywords
		 (null syntax-propertize-function))
	;; Ensure the beginning of the file is properly syntactic-fontified.
	(let ((start beg))
	  (when (< font-lock-syntactically-fontified start)
	    (setq start (max font-lock-syntactically-fontified (point-min)))
	    (setq font-lock-syntactically-fontified end))
	  (font-lock-fontify-syntactic-keywords-region start end)))
      (unless font-lock-keywords-only
	(font-lock-fontify-syntactically-region beg end loudly))
      (pjs-font-lock-fontify-keywords-region beg end loudly))))

(defun pjs-font-lock-fontify-keywords-region (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END.
START should be at the beginning of a line.
LOUDLY, if non-nil, allows progress-meter bar."
  (unless (eq (car font-lock-keywords) t)
    (setq font-lock-keywords
	  (font-lock-compile-keywords font-lock-keywords)))
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-keywords))
	(bufname (buffer-name)) (count 0)
        (pos (make-marker))
	keyword matcher highlights start-time l (total 0))
    ;;
    ;; Fontify each item in `font-lock-keywords' from `start' to `end'.
    (while keywords
      (setq start-time (float-time))
      ;;      (if loudly (message "Fontifying %s... (regexps..%s)" bufname
      ;;			  (make-string (cl-incf count) ?.)))
      ;;
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (and (< (point) end)
		  (if (stringp matcher)
		      (re-search-forward matcher end t)
		    (funcall matcher end))
                  ;; Beware empty string matches since they will
                  ;; loop indefinitely.
                  (or (> (point) (match-beginning 0))
                      (progn (forward-char 1) t)))
	(when (and font-lock-multiline
		   (>= (point)
		       (save-excursion (goto-char (match-beginning 0))
				       (forward-line 1) (point))))
	  ;; this is a multiline regexp match
	  ;; (setq font-lock-multiline t)
	  (put-text-property (if (= (point)
				    (save-excursion
				      (goto-char (match-beginning 0))
				      (forward-line 1) (point)))
				 (1- (point))
			       (match-beginning 0))
			     (point)
			     'font-lock-multiline t))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-highlight (car highlights))
	    (set-marker pos (point))
            (font-lock-fontify-anchored-keywords (car highlights) end)
            ;; Ensure forward progress.  `pos' is a marker because anchored
            ;; keyword may add/delete text (this happens e.g. in grep.el).
            (if (< (point) pos) (goto-char pos)))
	  (setq highlights (cdr highlights))))
      ;; on fait le bilan, calmement
      (when loudly
	(let ((time (- (float-time) start-time)))
	  (setq total (+ total time))
	  (push (list (cl-incf count) matcher time 0) l)))
      (setq keywords (cdr keywords)))
    ;; on fait le bilan, calmement
    (when loudly
      (message "Fontifying buffer %s from %s to %s. Total time %f"
	       (current-buffer)
	       start
	       end
	       total)
      (dolist (k l)
	(setf (fourth k) (* 100 (/ (third k) total))))
      (dolist (k (sort l '(lambda (a b) (> (fourth a) (fourth b)))))
	(message "Rule No %s (type %s) : Run time %f : %f %%"
		 (car k)
		 (if (stringp (second k))
		     (format "Regexp of size %s (%s)" (length (second k))
			     (substring (second k) 0 (min (length (second k)) 20)))
		   (format "Function %s" (second k)))
		 (third k)
		 (fourth k))))
    
    (set-marker pos nil)))
