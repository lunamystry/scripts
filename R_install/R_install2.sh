#! /bin/bash
#
# brief: An installation script for R
#
# author: Leonard Mandla Mbuli
#
# created: 24 November 2014
# last updated: 24 November 2014
# version: 0.0.2

echo "Removing old R installation..."
sudo apt-get -q remove r-base-core

echo "Adding R source..."
echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" | sudo tee /etc/apt/sources.list.d/R.list

echo "Adding R source GPG key..."
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -

echo "Update package list..."
sudo apt-get update

echo "Upgrade system..."
sudo apt-get dist-upgrade

echo "Install R packages..."
sudo apt-get install r-base r-base-core r-base-html r-bioc-biocinstaller \
    r-bioc-genomicranges r-bioc-qvalue r-cran-genetics
