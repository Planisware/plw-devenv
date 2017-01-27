#!/bin/bash
## -*- coding: windows-1252 -*- 
##  COPYRIGHT (C) PLANISWARE 2017
##
##  All Rights Reserved
##
##  This program and the information contained herein are confidential to
##  and the property of PLANISWARE and are made available only to PLANISWARE
##  employees for the sole purpose of conducting PLANISWARE business.
##
##**************************************************************************

DEST_DIR=$1
SRC_DIR=../
FILES=el/files
DIRS=el/directories
EXTENSIONS="el txt"

if [ -z $1 ] 
then
    echo "Destination dir missing";
    exit -1;
fi

if [ ! -f Makefile ]
then
    echo "Makefile not found ! This script should be run in the runtime directory directly"
    exit -1;
fi

mkdir -p $DEST_DIR

# copy files
while read LINE; do
    SUBDIR=`echo $LINE | awk -F"/" 'BEGIN {OFS = FS}/.*\/.*/{$NF=""; print $0}'`
    if [ ! -d "$DEST_DIR$SUBDIR" ]; then
	mkdir $DEST_DIR$SUBDIR
    fi
    echo "Copying $SRC_DIR$LINE to $DEST_DIR$SUBDIR"
    cp $SRC_DIR$LINE $DEST_DIR$SUBDIR
done < $FILES

# copy directories
while read LINE; do
    mkdir -p $DEST_DIR$LINE
    for EXT in $EXTENSIONS; do
        if ls $SRC_DIR$LINE*.$EXT 1> /dev/null 2>&1; 
        then
            cp $SRC_DIR$LINE*.$EXT $DEST_DIR$LINE
        fi
    done
done < $DIRS
