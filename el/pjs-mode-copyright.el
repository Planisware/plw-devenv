(defconst *pjs-copyright* "//* -*- Mode: pjs -*- *****************************************************
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
//  FILE    : __FILENAME__
//
//  VERSION : $Id$
//
//**************************************************************************
")

(defvar *pjs-copyright-lines-to-check* 20)

(defvar *pjs-copyright-head* (substring *pjs-copyright* 0 (with-temp-buffer
							    (insert *pjs-copyright*)
							    (goto-char (point-min))
							    (or (re-search-forward "\n" nil t *pjs-copyright-lines-to-check*) (point-max)))))

(defvar *pjs-copyright-footer* (substring *pjs-copyright* (with-temp-buffer
							    (insert *pjs-copyright*)
							    (goto-char (point-max))
							    (or (re-search-backward "\n" nil t 5) (point-max)))))
