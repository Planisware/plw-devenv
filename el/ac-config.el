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
;;;; $Log$
;;;; Revision 3.2  2015/06/18 08:32:28  troche
;;;; * configuration
;;;;  (header added automatically)
;;;;
(require 'auto-complete-config)

(ac-config-default)
(setq ac-auto-start nil)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.1)

;; always complete on shift tab
(define-key ac-mode-map (read-kbd-macro "<backtab>") (lambda () (interactive) (ac-trigger-key-command t)))
;;always complete on ctrl space
;;(define-key ac-mode-map (read-kbd-macro "C-SPC") (lambda () (interactive) (ac-trigger-key-command t)))

;; lisp modes
(add-to-list 'ac-modes 'fi:common-lisp-mode)
(add-to-list 'ac-modes 'fi:inferior-common-lisp-mode)
(add-to-list 'ac-modes 'fi:lisp-listener-mode)

(when *use-opx2-js-mode*
  ;; custom dictionaries
  (add-to-list 'ac-dictionary-directories (fullpath-relative-to-current-file "../packages/ac-dict"))

  ;; js modes
  (add-to-list 'ac-modes 'opx2-js-mode)
  (add-to-list 'ac-modes 'pjs-mode)
  (add-to-list 'ac-modes 'js-evaluator-mode))
