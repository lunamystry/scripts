#! /bin/bash
#
# brief: An installation script for R
#
# author: Leonard Mandla Mbuli
#
# created: 24 November 2014
# last updated: 24 November 2014
# version: 0.0.1

set -e # exit if anything goes wrong
VERSION=3.1.2
TEMP_DIR=/tmp/R # This will be deleted afterwards
export TMPDIR=$TEMP_DIR

sudo apt-get install build-essential cdbs debhelper dh-apparmor dh-translations \
 dpkg-dev gfortran gfortran-4.8 intltool libalgorithm-diff-perl \
 libalgorithm-diff-xs-perl libalgorithm-merge-perl libblas-dev libbz2-dev \
 libgfortran-4.8-dev libjpeg-dev libjpeg-turbo8-dev libjpeg8-dev liblapack-dev \
 libncurses5-dev libreadline-dev libreadline6-dev libtcl8.5 libtinfo-dev \
 libtk8.5 python-scour tcl8.5 tk8.5 texinfo

CURR_DIR=$(pwd)
if [ ! -d $TEMP_DIR ]; then
    mkdir $TEMP_DIR -p
fi
cd $TEMP_DIR

if [ ! -f R-$VERSION.tar.gz ]; then
  wget http://cran.mirror.ac.za/src/base/R-3/R-$VERSION.tar.gz
fi

if [ ! -d R-$VERSION ]; then
  tar -xf R-$VERSION.tar.gz
fi

cd R-$VERSION
ls
./configure --with-x=yes --enable-R-shlib
make
make check

sudo make uninstall
# sudo make install-info

cd $CURR_DIR
rm --force --recursive $TEMP_DIR
