#! /bin/bash
#
# brief: An installation script for plink, used with R
#
# author: Leonard Mandla Mbuli
#
# created: 24 November 2014
# last updated: 24 November 2014
# version: 0.0.1

set -e # exit if anything goes wrong
VERSION=linux_x86_64
BIN_DIR=/usr/local/bin
INSTALL_DIR=/opt/plink
TEMP_DIR=/tmp/plink # This will be deleted afterwards

CURR_DIR=$(pwd)
if [ ! -d $TEMP_DIR ]; then
    mkdir $TEMP_DIR -p
fi
cd $TEMP_DIR

if [ ! -f plink_$VERSION.zip ]; then
  wget https://www.cog-genomics.org/static/bin/plink141120/plink_$VERSION.zip
fi

if [ ! -d plink_$VERSION ]; then
  unzip -d plink_$VERSION plink_$VERSION.zip
fi

if [ ! -d /opt/plink_$VERSION ]; then
  sudo mv plink_$VERSION /opt
fi
sudo ln /opt/plink_$VERSION/plink $BIN_DIR/plink
sudo ln /opt/plink_$VERSION/prettify $BIN_DIR/prettify

cd $CURR_DIR
rm --force --recursive $TEMP_DIR
