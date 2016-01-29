(defun font-lock-fontify-keywords-region (start end &optional loudly)
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
	keyword matcher highlights)
    ;;
    ;; Fontify each item in `font-lock-keywords' from `start' to `end'.
    (while keywords
      (if loudly (message "Fontifying %s... (regexps..%s)" bufname
			  (make-string (cl-incf count) ?.)))
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
      (setq keywords (cdr keywords)))
    (set-marker pos nil)))


(defun pjs-semantic-edits-incremental-parser-1 ()
  (let* ((changed-tags nil)
         (debug-on-quit t)            ; try to find this annoying bug!
         (changes (semantic-changes-in-region
                   (point-min) (point-max)))
         (tags nil)		   ;tags found at changes
         (newf-tags nil)	   ;newfound tags in change
         (parse-start nil)	   ;location to start parsing
         (parse-end nil)	   ;location to end parsing
         (parent-tag nil)	   ;parent of the cache list.
         (cache-list nil)	   ;list of children within which
					;we incrementally reparse.
         (reparse-symbol nil)	   ;The ruled we start at for reparse.
         (change-group nil)	   ;changes grouped in this reparse
	 (last-cond nil)	   ;track the last case used.
					;query this when debugging to find
					;source of bugs.
         )
    (or changes
        ;; If we were called, and there are no changes, then we
        ;; don't know what to do.  Force a full reparse.
        (semantic-parse-changes-failed "Don't know what to do"))
    ;; Else, we have some changes.  Loop over them attempting to
    ;; patch things up.
    (while changes
      ;; Calculate the reparse boundary.
      ;; We want to take some set of changes, and group them
      ;; together into a small change group. One change forces
      ;; a reparse of a larger region (the size of some set of
      ;; tags it encompasses.)  It may contain several tags.
      ;; That region may have other changes in it (several small
      ;; changes in one function, for example.)
      ;; Optimize for the simple cases here, but try to handle
      ;; complex ones too.

      (while (and changes               ; we still have changes
                  (or (not parse-start)
                      ;; Below, if the change we are looking at
                      ;; is not the first change for this
                      ;; iteration, and it starts before the end
                      ;; of current parse region, then it is
                      ;; encompassed within the bounds of tags
                      ;; modified by the previous iteration's
                      ;; change.
                      (< (semantic-overlay-start (car changes))
                         parse-end)))

        ;; REMOVE LATER
        (if (eq (car changes) (car change-group))
            (semantic-parse-changes-failed
             "Possible infinite loop detected"))

        ;; Store this change in this change group.
        (setq change-group (cons (car changes) change-group))

        (cond
         ;; Is this is a new parse group?
         ((not parse-start)
	  (setq last-cond "new group")
          (let (tmp)
            (cond

;;;; Are we encompassed all in one tag?
             ((setq tmp (semantic-edits-change-leaf-tag (car changes)))
	      (setq last-cond "Encompassed in tag")
              (setq tags (list tmp)
                    parse-start (semantic-tag-start tmp)
                    parse-end (semantic-tag-end tmp)
                    )
	      (semantic-edits-assert-valid-region))

;;;; Did the change occur between some tags?
             ((setq cache-list (semantic-edits-change-between-tags
                                (car changes)))
	      (setq last-cond "Between and not overlapping tags")
              ;; The CAR of cache-list is the tag just before
              ;; our change, but wasn't modified.  Hmmm.
              ;; Bound our reparse between these two tags
              (setq tags nil
                    parent-tag
                    (car (semantic-find-tag-by-overlay
                          parse-start)))
              (cond
               ;; A change at the beginning of the buffer.
	       ;; Feb 06 -
	       ;; IDed when the first cache-list tag is after
	       ;; our change, meaning there is nothing before
	       ;; the change.
               ((> (semantic-tag-start (car cache-list))
                   (semantic-overlay-end (car changes)))
		(setq last-cond "Beginning of buffer")
                (setq parse-start
                      ;; Don't worry about parents since
                      ;; there there would be an exact
                      ;; match in the tag list otherwise
                      ;; and the routine would fail.
                      (point-min)
                      parse-end
                      (semantic-tag-start (car cache-list)))
		(semantic-edits-assert-valid-region)
                )
               ;; A change stuck on the first surrounding tag.
               ((= (semantic-tag-end (car cache-list))
                   (semantic-overlay-start (car changes)))
		(setq last-cond "Beginning of Tag")
                ;; Reparse that first tag.
                (setq parse-start
                      (semantic-tag-start (car cache-list))
                      parse-end
                      (semantic-overlay-end (car changes))
                      tags
                      (list (car cache-list)))
		(semantic-edits-assert-valid-region)
                )
               ;; A change at the end of the buffer.
               ((not (car (cdr cache-list)))
		(setq last-cond "End of buffer")
                (setq parse-start (semantic-tag-end
                                   (car cache-list))
                      parse-end (point-max))
		(semantic-edits-assert-valid-region)
                )
               (t
		(setq last-cond "Default")
                (setq parse-start
                      (semantic-tag-end (car cache-list))
                      parse-end
                      (semantic-tag-start (car (cdr cache-list)))
                      )
		(semantic-edits-assert-valid-region))))

;;;; Did the change completely overlap some number of tags?
             ((setq tmp (semantic-edits-change-over-tags
                         (car changes)))
	      (setq last-cond "Overlap multiple tags")
              ;; Extract the information
              (setq tags (aref tmp 0)
                    cache-list (aref tmp 1)
                    parent-tag (aref tmp 2))
              ;; We can calculate parse begin/end by checking
              ;; out what is in TAGS.  The one near start is
              ;; always first.  Make sure the reparse includes
              ;; the `whitespace' around the snarfed tags.
              ;; Since cache-list is positioned properly, use it
              ;; to find that boundary.
              (if (eq (car tags) (car cache-list))
                  ;; Beginning of the buffer!
                  (let ((end-marker (nth (length tags)
                                         cache-list)))
                    (setq parse-start (point-min))
                    (if end-marker
                        (setq parse-end
                              (semantic-tag-start end-marker))
                      (setq parse-end (semantic-overlay-end
                                       (car changes))))
		    (semantic-edits-assert-valid-region)
		    )
                ;; Middle of the buffer.
                (setq parse-start
                      (semantic-tag-end (car cache-list)))
                ;; For the end, we need to scoot down some
                ;; number of tags.  We 1+ the length of tags
                ;; because we want to skip the first tag
                ;; (remove 1-) then want the tag after the end
                ;; of the list (1+)
                (let ((end-marker (nth (1+ (length tags)) cache-list)))
                  (if end-marker
                      (setq parse-end (semantic-tag-start end-marker))
                    ;; No marker.  It is the last tag in our
                    ;; list of tags.  Only possible if END
                    ;; already matches the end of that tag.
                    (setq parse-end
                          (semantic-overlay-end (car changes)))))
		(semantic-edits-assert-valid-region)
                ))

;;;; Unhandled case.
             ;; Throw error, and force full reparse.
             ((semantic-parse-changes-failed (format "Unhandled change group. Last cond : %s" last-cond))))
            ))
         ;; Is this change inside the previous parse group?
         ;; We already checked start.
         ((< (semantic-overlay-end (car changes)) parse-end)
	  (setq last-cond "in bounds")
          nil)
         ;; This change extends the current parse group.
         ;; Find any new tags, and see how to append them.
         ((semantic-parse-changes-failed
	   (setq last-cond "overlap boundary")
           "Unhandled secondary change overlapping boundary"))
         )
        ;; Prepare for the next iteration.
	(message "last : %s" last-cond)
        (setq changes (cdr changes)))

      ;; By the time we get here, all TAGS are children of
      ;; some parent.  They should all have the same start symbol
      ;; since that is how the multi-tag parser works.  Grab
      ;; the reparse symbol from the first of the returned tags.
      ;;
      ;; Feb '06 - If reparse-symbol is nil, then they are top level
      ;;     tags.  (I'm guessing.)  Is this right?
      (setq reparse-symbol
            (semantic--tag-get-property (car (or tags cache-list))
                                        'reparse-symbol))
      ;; Find a parent if not provided.
      (and (not parent-tag) tags
           (setq parent-tag
                 (semantic-find-tag-parent-by-overlay
                  (car tags))))
      ;; We can do the same trick for our parent and resulting
      ;; cache list.
      (unless cache-list
	(if parent-tag
	    (setq cache-list
		  ;; We need to get all children in case we happen
		  ;; to have a mix of positioned and non-positioned
		  ;; children.
		  (semantic-tag-components parent-tag))
	  ;; Else, all the tags since there is no parent.
	  ;; It sucks to have to use the full buffer cache in
	  ;; this case because it can be big.  Failure to provide
	  ;; however results in a crash.
	  (setq cache-list semantic--buffer-cache)
	  ))
      ;; Use the boundary to calculate the new tags found.
      (setq newf-tags (semantic-parse-region
		       parse-start parse-end reparse-symbol))
      
      ;; we failed, do nothing
      (when (eq newf-tags :error)
	(throw 'semantic-parse-changes-failed :error))
      
      ;; Make sure all these tags are given overlays.
      ;; They have already been cooked by the parser and just
      ;; need the overlays.
      (let ((tmp newf-tags))
        (while tmp
          (semantic--tag-link-to-buffer (car tmp))
          (setq tmp (cdr tmp))))

      ;; See how this change lays out.
      (cond

;;;; Whitespace change
       ((and (not tags) (not newf-tags))
        ;; A change that occurred outside of any existing tags
        ;; and there are no new tags to replace it.
	(when semantic-edits-verbose-flag
	  (message "White space changes"))
        nil
        )

;;;; New tags in old whitespace area.
       ((and (not tags) newf-tags)
        ;; A change occurred outside existing tags which added
        ;; a new tag.  We need to splice these tags back
        ;; into the cache at the right place.
        (semantic-edits-splice-insert newf-tags parent-tag cache-list)

        (setq changed-tags
              (append newf-tags changed-tags))

	(when semantic-edits-verbose-flag
	  (message "Inserted tags: (%s)"
		   (semantic-format-tag-name (car newf-tags))))
        )

;;;; Old tags removed
       ((and tags (not newf-tags))
        ;; A change occurred where pre-existing tags were
        ;; deleted!  Remove the tag from the cache.
        (semantic-edits-splice-remove tags parent-tag cache-list)

        (setq changed-tags
              (append tags changed-tags))

        (when semantic-edits-verbose-flag
	  (message "Deleted tags: (%s)"
		   (semantic-format-tag-name (car tags))))
        )

;;;; One tag was updated.
       ((and (= (length tags) 1) (= (length newf-tags) 1))
        ;; One old tag was modified, and it is replaced by
        ;; One newfound tag.  Splice the new tag into the
        ;; position of the old tag.
        ;; Do the splice.
        (semantic-edits-splice-replace (car tags) (car newf-tags))
        ;; Add this tag to our list of changed toksns
        (setq changed-tags (cons (car tags) changed-tags))
        ;; Debug
        (when semantic-edits-verbose-flag
	  (message "Update Tag Table: %s"
		   (semantic-format-tag-name (car tags) nil t)))
        ;; Flush change regardless of above if statement.
        )

;;;; Some unhandled case.
       ((semantic-parse-changes-failed "Don't know what to do")))

      ;; We got this far, and we didn't flag a full reparse.
      ;; Clear out this change group.
      (while change-group
        (semantic-edits-flush-change (car change-group))
        (setq change-group (cdr change-group)))

      ;; Don't increment change here because an earlier loop
      ;; created change-groups.
      (setq parse-start nil)
      )
    ;; Mark that we are done with this glop
    (semantic-parse-tree-set-up-to-date)
    ;; Return the list of tags that changed.  The caller will
    ;; use this information to call hooks which can fix themselves.
    changed-tags))

(defun pjs-semantic-edits-change-leaf-tag (change)
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
