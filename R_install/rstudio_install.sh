#! /bin/bash
#
# brief: An installation script for GCTA, used with R
#
# author: Leonard Mandla Mbuli
#
# created: 24 November 2014
# last updated: 24 November 2014
# version: 0.0.1

set -e # exit if anything goes wrong
VERSION=0.98.1091-amd64
BIN_DIR=/usr/local/bin
TEMP_DIR=/tmp/rstudio # This will be deleted afterwards

CURR_DIR=$(pwd)
if [ ! -d $TEMP_DIR ]; then
    mkdir $TEMP_DIR -p
fi
cd $TEMP_DIR

if [ ! -f rstudio-$VERSION.deb ]; then
  wget http://download1.rstudio.org/rstudio-$VERSION.deb
fi

sudo dpkg -i rstudio-$VERSION.deb

cd $CURR_DIR
rm --force --recursive $TEMP_DIR
