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
VERSION=1.24.4
BIN_DIR=/usr/local/bin
INSTALL_DIR=/opt/gcta
TEMP_DIR=/tmp/gcta # This will be deleted afterwards

CURR_DIR=$(pwd)
if [ ! -d $TEMP_DIR ]; then
    mkdir $TEMP_DIR -p
fi
cd $TEMP_DIR

if [ ! -f gcta_$VERSION.zip ]; then
  wget http://www.complextraitgenomics.com/software/gcta/gcta_$VERSION.zip
fi

if [ ! -d gcta_$VERSION ]; then
  unzip -d gcta_$VERSION gcta_$VERSION.zip
fi

if [ ! -d /opt/gcta_$VERSION ]; then
  sudo mv gcta_$VERSION /opt
fi
sudo ln /opt/gcta_$VERSION/gcta64 $BIN_DIR/gcta64

cd $CURR_DIR
rm --force --recursive $TEMP_DIR
