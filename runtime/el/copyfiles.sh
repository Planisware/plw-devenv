#!/bin/bash
DEST_DIR=$1
SRC_DIR=../../
FILES=files
DIRS=directories
EXTENSIONS="el txt"

# copy files
while read LINE; do    
    cp $SRC_DIR$LINE $DEST_DIR
done < $FILES

# copy directories
while read LINE; do
    mkdir -p $DEST_DIR$LINE
    for EXT in $EXTENSIONS; do
        cp $SRC_DIR$LINE*.$EXT $DEST_DIR$LINE
    done
done < $DIRS
