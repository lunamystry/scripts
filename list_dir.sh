#! /bin/bash
#
# brief: <description> 
#
# author: Leonard Mandla Mbuli
#
# created: <creation date>
# last updated: <last updated>
# version: <current version>

PROGNAME=$(basename $0)
VERSION="0.0.1"

function usage
{
    echo "Usage: ${PROGNAME} [-h | --help] [-d directory] [-o outputfile]"
}

function showhelp
{
    local tab=$(echo -en "\t\t")

    cat <<- -EOF-

    ${PROGNAME} ver. ${VERSION}
    This is a program to demonstrate the output of new_script.

    $(usage)

    Options:

    -h, --help Display this help message and exit.
    -d  directory the directory to list
    -o  outputfile the outfile

-EOF-
}

function getcontent
{
    curr_dir=$(pwd)
    cd $1
    fullpath=$(pwd)
    fullpath=${fullpath//\//\_}
    cd $curr_dir
    \ls $1 | tee -a contents_of$fullpath.txt
}

if [ "$1" = "--help" ]; then
    showhelp
fi

if [ -d "$1" ]; then
    getcontent $1
fi

while getopts ":hd:o:" opt; do
    case $opt in
        d ) getcontent $OPTARG ;;
        h ) showhelp;;
        * ) usage
            exit 1;;
    esac
done

for filename in *
do
    if [ -d $filename ]; then
        cd $filename
        chmod 664 *
        cd ..
    fi
done
