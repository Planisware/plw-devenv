;;;; -*- coding: windows-1252 -*-
;;;; COPYRIGHT (C) PLANISWARE $Date$ 
;;;;
;;;; All Rights Reserved
;;;;
;;;; This program and the information contained herein are confidential to
;;;; and the property of PLANISWARE and are made available only to PLANISWARE
;;;; employees for the sole purpose of conducting PLANISWARE business.
;;;;
;;;; This program and copy therof and the information contained herein shall
;;;; be maintained in strictest confidence ; shall not be copied in whole or
;;;; in part except as authorized by the employee's manager ; and shall not
;;;; be disclosed or distributed (a) to persons who are not PLANISWARE employees,
;;;; or (b) to PLANISWARE employees for whom such information is not necessary in
;;;; connection with their assigned responsabilities.
;;;;
;;;; There shall be no exceptions to the terms and conditions set forth
;;;; herein except as authorized in writing by the responsible PLANISWARE General
;;;; Manager.

;;;;
;;;; FILE    : $RCSfile$
;;;;
;;;; AUTHOR  : $Author$
;;;;
;;;; VERSION : $Id$
;;;;
;;;; PURPOSE :
;;;;
;;;; (when (fboundp :set-source-info) (:set-source-info "$RCSfile$" :id "$Id$" :version "$Revision$" :date "$Date$ "))
;;;; (when (fboundp :doc-patch) (:doc-patch ""))
;;;; (:require-patch "")
;;;; HISTORY :

;;;; Revision 3.1  2016/03/21 13:22:46  troche
;;;; * new files
;;;;  (header added automatically)
;;;;
(require 'auto-complete)

;;;;; configuration of the hook

;;; complete plc. classes names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "pjs-classes"
  '((prefix . pjs-classes-ac-prefix)
    (candidates . pjs-classes-ac-candidates)
;;    (document . pjs-namespaces-functions-ac-document)
;;    (action . pjs-namespaces-functions-ac-action)
    (symbol . "c ")
    (requires . -1)))

(defun pjs-classes-ac-prefix ()
  ;; detects the current namespace
  (save-excursion
    (save-match-data
      (let ((match (re-search-backward (format "\\(\\<plc\\>\\)\\(\\.\\)\\(%s\\)?\\=" *js-variable-name*) (line-beginning-position) t)))
	(when match
;;	  (setq *current-pjs-namespace-prefix* "plc")
	  (1+ (match-beginning 2)))))))

(defun pjs-classes-ac-candidates ()
  (list-pjs-plc-types))

;;; find functions and variables from  namespaces.
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
  (save-excursion
    (save-match-data
      (let ((match (re-search-backward (format "\\(\\<%s\\>\\)\\(\\.\\)\\(%s\\)?\\=" *js-variable-name* *js-variable-name*) (line-beginning-position) t)))
	(when (and match
		   (not (string= (downcase (match-string-no-properties 1)) "plc")))
	  (setq *current-pjs-namespace-prefix* (downcase (match-string-no-properties 1)))
	  (1+ (match-beginning 2)))))))

(defun pjs-namespaces-functions-ac-action ()  
  )

(defun pjs-namespaces-functions-ac-candidates ()
  (let ((functions (list-pjs-namespace-functions *current-pjs-namespace-prefix*))
	(vars      (list-pjs-namespace-variables *current-pjs-namespace-prefix*)))
    (append functions vars)))

(defun pjs-namespaces-functions-ac-document (docstr)
  docstr)

;;; find functions from current namespaces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "pjs-current-namespace-functions"
  '((candidates . pjs-current-namespace-functions-ac-candidates)
;;    (document . pjs-namespaces-functions-ac-document)
    ;;    (action . pjs-namespaces-functions-ac-action)
    (symbol . "f ")
    (requires . -1)))

(defun pjs-current-namespace-functions-ac-candidates ()
  (let ((functions (list-pjs-namespace-functions (pjs-current-namespace)))
	(vars      (list-pjs-namespace-variables (pjs-current-namespace))))
    (append functions vars)))

(defun pjs-current-namespace-functions-ac-document (docstr)
  docstr)


;;; completion for typed variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ac-define-source "pjs-variable-members"
  '((prefix . pjs-variable-members-ac-prefix)
    (candidates . pjs-variable-members-ac-candidates)
;;    (document . pjs-namespaces-functions-ac-document)
;;    (action . pjs-namespaces-functions-ac-action)
    (symbol . "m ")
    (requires . -1)))

(defvar *current-pjs-variable* nil)

(defun pjs-variable-members-ac-prefix ()
  ;; detects the current namespace
  (save-excursion
    (save-match-data
      (let ((match (re-search-backward (format "\\(\\<%s\\>\\)\\([.]\\)\\(%s\\)?\\=" *js-variable-name* *js-variable-name*) (line-beginning-position) t)))	
	(when match
	  (1+ (setq *current-pjs-variable* (match-beginning 2))))))))

(defun pjs-variable-members-ac-candidates ()
  ;; get the type of the variable
  (let ((var-type (get-variable-type-in-context *current-pjs-variable*))
	res)
    (when var-type
      (let* ((members (pjs-class-members (car var-type) (cdr var-type)))
	     (res (pjs-class-methods (car var-type) (cdr var-type))))
	(maphash #'(lambda (k v)
		     (push k res)) members)
	res))))

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
  (add-to-list 'ac-sources 'ac-source-pjs-current-namespace-functions)
  (add-to-list 'ac-sources 'ac-source-pjs-local-vars)
  (add-to-list 'ac-sources 'ac-source-pjs-variable-members)
  (add-to-list 'ac-sources 'ac-source-pjs-classes)
 
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

(provide 'ac-pjs)
