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
(defconst *plw-copyright* "//* -*- Mode: __MODE__ -*- *****************************************************
//  COPYRIGHT (C) PLANISWARE 
//
//  All Rights Reserved
//
//  This program and the information contained herein are confidential to
//  and the property of PLANISWARE and are made available only to PLANISWARE
//  employees for the sole purpose of conducting PLANISWARE business.
//
//  This program and copy therof and the information contained herein shall
//  be maintained in strictest confidence ; shall not be copied in whole or
//  in part except as authorized by the employee's manager ; and shall not
//  be disclosed or distributed (a) to persons who are not PLANISWARE employees,
//  or (b) to PLANISWARE employees for whom such information is not necessary in
//  connection with their assigned responsabilities.
//
//  There shall be no exceptions to the terms and conditions set forth
//  herein except as authorized in writing by the responsible PLANISWARE General
//  Manager.
//
//
// FILE    : __FILENAME__
// 
//**************************************************************************")

(defvar *plw-copyright-lines-to-check* 20)

(defvar *plw-copyright-head* (substring *plw-copyright* 0 (with-temp-buffer
							    (insert *plw-copyright*)
							    (goto-char (point-min))
							    (or (re-search-forward "\n" nil t *plw-copyright-lines-to-check*) (point-max)))))
