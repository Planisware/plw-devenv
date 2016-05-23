(in-package :intranet)

(defvar *emacs-runtime-mode* nil)

(defvar *emacs-runtime-initial-bindings 
    (list
     'intranet::*emacs-runtime-mode* t
     'sockets::*verbose-startup* nil
     'sockets::*running-under-watchdog* nil
;;;     'common-lisp-user::*compilation-mode* :runtime-with-macro
;;;     'foreign-file::*opx2-temporary-path* #+UNIX (format nil "~A~A/emacs/" (sys::temporary-directory) (foreign-interface::get-login))
;;;     #-UNIX (format nil "~Aemacs/" (sys::temporary-directory))
     ))

(defun :start-emacs-runtime-mode ()
  (doplist (var value *emacs-runtime-initial-bindings)
    (set var value))
  (startup::load-foreign-code) 
  (load "intranet.ini") 
  (:require-patch "sc9404")
  (intranet::main))
