;; -*- coding: windows-1252 -*- 
;;  COPYRIGHT (C) PLANISWARE 2017
;;
;;  All Rights Reserved
;;
;;  This program and the information contained herein are confidential to
;;  and the property of PLANISWARE and are made available only to PLANISWARE
;;  employees for the sole purpose of conducting PLANISWARE business.
;;
;;**************************************************************************

(in-package :intranet)

(defvar *emacs-runtime-mode* nil)

(defvar *emacs-runtime-initial-bindings 
    (list
     'intranet::*emacs-runtime-mode* t
     'sockets::*verbose-startup* nil
     'sockets::*running-under-watchdog* nil
     ))

(defun :start-emacs-runtime-mode ()
  (doplist (var value *emacs-runtime-initial-bindings)
    (set var value))
  (:r)
  (startup::load-foreign-code) 
  (load "intranet.ini") 
  (:require-patch "sc9404")
  ;;(:enable-debugger)
  (intranet::main))
