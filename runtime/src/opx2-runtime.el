
;; we will generate a command and an entry per version listed here
(defvar *opx2-installations-paths* nil)
;;  '( "600SP0" "/home/troche/opx2/600SP0install/modules/bin/Lx86_64/"
;;     "610SP1" "/home/troche/opx2/install/modules/bin/Lx86_64/"))

(defvar *opx2-installations-paths-conf-file* "planisware-versions.conf")

(defvar *runtime-function-name*
  "runopx2%s")

(defvar *runtime-function-body*
  "(defun %s ()
    (interactive)
    (runopx2runtime \"%s\"))")

(defvar *runtime-exe* (if (on-ms-windows) "intranet.exe" "opx2-intranet.exe"))

(defvar *runtime-dxl* (if (on-ms-windows) "opx2-intranet.dxl" "opx2-intranet.dxl"))

(defvar *planisware-menu-name* "Planisware")

(defvar *start-planisware-menu-item* "Start Planisware %s ...")

(defvar *runtime-verbose* t)

(defun runopx2runtime (rootdir)
  (catch 'exit
    (let* ((intranetini (read-file-name "Please enter the location of your intranet.ini file : " "~" "intranet.ini" t))
	   (satdir      (subseq intranetini 0 (min (1+ (position ?/ intranetini :from-end t))
						   (length intranetini)))))
      (unless (string-match ".*/intranet.ini" intranetini)
	(message "%s is not a valid path to an intranet.ini" intranetini)
	(throw 'exit nil))
      (unless (file-exists-p satdir)
	(message "Directory %s not found ! " satdir)
	(throw 'exit nil))
      (when *runtime-verbose*
	(message "Starting in directory %s" satdir))
      (setq fi::started-via-file nil)
      (fi:common-lisp fi:common-lisp-buffer-name
		      satdir
		      ;;		    "/usr/local/acl90-smp.64/alisp8"
		      (format "%s%s" rootdir *runtime-exe*)
		      ;;'("-e" "(setq *dede* t)")
		      (nconc *start-emacs-lisp-interface*
			     (list
			      "-H" rootdir
			      "-L" (format "%s/emacs-runtime.lisp" *opx2-network-folder-work-path*)
			      ;;			     "-e" "(setq excl::*restart-app-function* :start-emacs-runtime-mode)"
			      "-e" "(setq excl::*restart-app-function* nil)"
			      ))
		      fi:common-lisp-host
		      (format "%s%s" rootdir *runtime-dxl*))
      (process-send-string fi:common-lisp-buffer-name "(:start-emacs-runtime-mode)\n")
      (switch-to-buffer fi:common-lisp-buffer-name))))

(defun load-version-configuration-file()
  (let ((file (format "%s/%s" *opx2-network-folder-work-path* *opx2-installations-paths-conf-file*))
	res
	(morelines t))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while morelines
	  (unless (fast-looking-at "#")
	    (let ((list (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
	      (when (>= (length list) 2)
		(cond ((file-exists-p (second list))
		       (push (first list) res)
		       (push (second list) res))
		      (t
		       (message "Ignoring version %s because directory %s does not exist." (first list) (second list)))))))
	  (setq morelines (= 0 (forward-line 1)))))
      (setq *opx2-installations-paths* (reverse res)))))

(defun generate-runtime-functions ()
  (cond ((load-version-configuration-file)  
	 (define-key-after
	   global-map
	   [menu-bar plw]
	   (cons *planisware-menu-name* (make-sparse-keymap "plw"))
	   'tools )
	 
	 (do* ((i 0 (+ i 2))
	       (version (nth i *opx2-installations-paths*) (nth i *opx2-installations-paths*))
	       (path    (nth (1+ i) *opx2-installations-paths*) (nth (1+ i) *opx2-installations-paths*)))
	     ((>= i (length *opx2-installations-paths*)))
	   (let ((funname (format *runtime-function-name* version)))
	     ;; generate the run function
	     (with-temp-buffer
	       (insert (format *runtime-function-body*
			       funname
			       path))
	       (eval-buffer))
	     ;; menu bar
	     (define-key
	       global-map
	       (vector 'menu-bar 'plw (intern funname))
	       (cons (format *start-planisware-menu-item* version) (intern funname))))))
	(t
	 (message "%s file not found or incorrect !" *opx2-installations-paths-conf-file*))))

(generate-runtime-functions)
