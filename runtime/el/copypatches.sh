#!/bin/bash
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
DEST_DIR=$1
PATCHES=el/patches
PATCHES_SOURCE=/E/versions/plw6/release/610SP1/updates/_en_dev/
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

# copy patches
while read LINE; do    
    cp $PATCHES_SOURCE$LINE"-is.obin" $DEST_DIR
done < $PATCHES
