#! /bin/bash
#
# brief: This script will bootstrap salt master or minion on Ubuntu or
# OpenSUSE/SLES depending on the option selected.
#
# author: Leonard Mandla Mbuli
#
# created: 1 December 2014
# version: 0.1.0

PROGNAME=$(basename $0)
VERSION="0.1.0"

function usage
{
    echo "Usage: ${PROGNAME} [ubuntu | suse] [master | minion] [-h | --help]"
}

function showhelp
{
    local tab=$(echo -en "\t\t")

    cat <<- -EOF-

    ${PROGNAME} ver. ${VERSION}
    This script will bootstrap salt master or minion on ubuntu or openSUSE/SLES

    $(usage)

    Options:

    -h, --help Display this help message and exit.
    ubuntu - Bootstrap on Ubuntu
    suse - Bootstrap on openSUSE/SLES
    master
    minion

-EOF-
}

function ubuntu
{
    echo "Ubuntu not yet supported..."
}

function suse
{
    echo "Boostrapping on openSUSE/SLES..."
    http="http:\/\/" # I will clean up input properly later... maybe
    default_proxy="146.141.119.192:3128"
    echo -n -e "\t proxy string for /etc/sysconfig/proxy [$default_proxy]: "
    read proxy
    if [ "$proxy" == "" ]; then
        proxy=$http$default_proxy
    fi
    proxy_config_template='suse_proxy.template'
    proxy_config='output.file'

    while read LINE; do
      echo $LINE |
      sed "s/{{ proxy }}/"$proxy"/g" >> $proxy_config
    done < $proxy_config_template
    echo -e "\t wrote proxy config to: $proxy_config"

    echo -e "\t add python repo..."
    sudo zypper addrepo http://download.opensuse.org/repositories/devel:/languages:/python/SLE_11_SP3/devel:languages:python.repo
    echo -e "\t add git repo..."
    sudo zypper addrepo http://download.opensuse.org/repositories/devel:/tools:/scm/SLE_11_SP3/devel:tools:scm.repo
    echo -e "\t add perl-Error repo..."
    sudo zypper addrepo http://download.opensuse.org/repositories/devel:/languages:/perl/SLE_11_SP3/devel:languages:perl.repo

    if [ "$1" = "master" ]; then
        echo ""
        echo -e "\t installing salt-master..."
        sudo zypper install salt-master
        sudo chkconfig salt-master on
        sudo rcsalt-master start
    elif [ "$1" = "minion" ]; then
        echo ""
        echo -e "\t installing salt-minion..."
        default_id=""
        default_master="localhost"
        echo -n -e "\t minion id [$default_id]: "
        read id
        if [ "$id" == "" ]; then
            id=$default_id
        fi

        minion_config_template='minion.template'
        minion_config_intermediate='minion_intermediate.file'
        minion_config='minion_output.file'
        while read LINE; do
            echo $LINE |
            sed "s/{{ id }}/"$id"/g" >> $minion_config_intermediate
        done < $minion_config_template
        echo -e "\t wrote minion config to: $minion_config_intermediate"

        echo -n -e "\t minion master [$default_master]: "
        read master
        if [ "$master" == "" ]; then
            master=$default_master
        fi

        while read LINE; do
            echo $LINE |
            sed "s/{{ master }}/"$master"/g" >> $minion_config
        done < $minion_config_intermediate
        echo -e "\t wrote minion config to: $minion_config"
        echo -e "\t removed $minion_config_intermediate"
        rm $minion_config_intermediate

        sudo zypper install salt-minion
        sudo chkconfig salt-minion on
        sudo rcsalt-master start
    else
        usage
    fi

    echo ""
    echo "Done!"
}

if [ "$1" = "--help" ]; then
    showhelp
    exit 1
fi

if [ "$1" = "suse" ]; then
    suse $2
elif [ "$1" = "ubuntu" ]; then
    ubuntu $2
fi

while getopts ":h" opt; do
    case $opt in
        h ) showhelp;;
        * ) usage
            exit 1;;
    esac
done
