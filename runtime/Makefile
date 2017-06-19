## -*- coding: windows-1252 -*- 
##  COPYRIGHT (C) PLANISWARE 2016
##
##  All Rights Reserved
##
##  This program and the information contained herein are confidential to
##  and the property of PLANISWARE and are made available only to PLANISWARE
##  employees for the sole purpose of conducting PLANISWARE business.
##
##**************************************************************************
ZIP=/usr/bin/zip -q

SRC_DIR = src
DIST_DIR = dist
ZIP_DIR = zip
ELI_DIR = ../eli
EL_DIR = ../el

ifdef VERSION
	# do nothing
else ifdef BUILD_NUMBER
    VERSION = "incremental-${BUILD_NUMBER}"
else 
    VERSION = "incremental-$(shell git describe --tags)"
endif

ZIPFILE="emacs-runtime-${VERSION}.zip"

ZIPTMPDIR=/tmp/runtime_`whoami`

clean:
	@@rm -rf ${DIST_DIR}

init: 
	@@test -d ${DIST_DIR} || mkdir ${DIST_DIR}
	@@test -d ${DIST_DIR}/devenv/ || mkdir ${DIST_DIR}/devenv/
	@@test -d ${DIST_DIR}/devenv/el || mkdir ${DIST_DIR}/devenv/el

copyfiles:
	@@echo "Building emacs runtime"

	@@echo "Copying source files"
	@@cp -r ${SRC_DIR}/* ${DIST_DIR}

	@@echo "Copying eli files"
#@@mkdir ${DIST_DIR}/eli
	@@cp -r ${ELI_DIR} ${DIST_DIR}

	@@echo "copying el files"
	@@cp -r ${EL_DIR}/* ${DIST_DIR}/devenv/el
	@@cp -r ../emacs-plw-ext.el ${DIST_DIR}/devenv/
#	@@sh ./el/copyfiles.sh ${dist_dir}/devenv/

patches:
	@@echo "copying required patches"
	@@sh ./el/copypatches.sh ${DIST_DIR}/patches/

version:
	@@echo "adding version.txt"
	@@echo ${VERSION} > ${DIST_DIR}/version.txt
	@@echo $(shell git describe --tags --abbrev=20) >> ${DIST_DIR}/version.txt

buildzip:
	@@rm -rf ${ZIP_DIR}
	@@mkdir -p ${ZIP_DIR}    
	@@echo "zip files to ${ZIPFILE}"
	@@rm -rf ${ZIPTMPDIR}
	@@mkdir -p ${ZIPTMPDIR}
	@@cp -r dist/* ${ZIPTMPDIR}
	@@cd ${ZIPTMPDIR}; ${ZIP} -r ${ZIPFILE} * -x \*cvs\*
	@@echo "move zip file to ${ZIP_DIR}."
	@@mv ${ZIPTMPDIR}/${ZIPFILE} ${ZIP_DIR}

zip: clean init copyfiles patches version buildzip

env: init copyfiles
