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
    cp --parents $SRC_DIR$LINE $DEST_DIR
done < $FILES
