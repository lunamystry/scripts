#! /usr/bin/env python

""" 
witsproxy: Sets the Wits proxy for all the files that I have to keep setting it for 

author: Leonard Mandla Mbuli <lm.mbuli@gmail.com>

creation date: 08 July 2011
last update: 29 July 2011
python 3

"""

import argparse
import re
import os
import sys
import fileinput

def main():
    """ The main function"""
    des = """Sets the Wits proxy for me, how kind don't you think\
          Needs to be run as root though """
    parser = argparse.ArgumentParser(description=des) 
    parser.add_argument('username',
                    help="The new or updated proxy username")
    parser.add_argument('password', 
                    help="The new or updated proxy password")
    parser.add_argument('-a', '--apt', action='store_true', 
                    help="Set only the apt proxy (Ubuntu or the like)")
    parser.add_argument('-b', '--bash', action='store_true', 
                    help="Set the bash proxy")
    parser.add_argument('-c', '--cntlm', action='store_true', 
                    help="Use CNTML to set the proxy (MUST ALREADY BE INSTALLED and configured to listern on port 8000)")
    parser.add_argument('-s', '--staff', action='store_true', 
                    help="Change proxy to staff proxy")
    args = parser.parse_args()
    proxystr = "http://"+args.username+":"+args.password

    if(args.staff)
        proxystr += "@proxyad.wits.ac.za:80/"
    elif(args.cntlm)
        # TODO: write the configuration file for CNTLM
        proxystr = "127.0.0.1:8000"
    else
        proxystr += "@proxyss.wits.ac.za:80/"

    if args.set_all == True or args.bash == True:
        set_bash_proxy(proxystr)
    if args.set_all == True or args.apt == True:
        set_apt_proxy(proxystr)

def set_apt_proxy(proxystr):
    """ 
    Sets the proxy for apt so I can install stuff
    Overides the set file
    """
    http_proxy = "Acquire::Proxy::Http \""+proxystr+"\";\n"
    https_proxy = "Acquire::Proxy::Https \""+proxystr+"\";\n"
    ftp_proxy = "Acquire::Proxy::Ftp \""+proxystr+"\";\n"

    f = open('/etc/apt/apt.conf', 'w')
    f.write(http_proxy)
    f.write(https_proxy)
    f.write(ftp_proxy)
    f.close()

def set_bash_proxy(proxystr):
    """ 
      Set the proxy in the .bashrc file

      - if does then replace them with the new ones
      - if it does not have the proxy str then append the string with message
      - if bashrc does not exist make it and append 
    """
    http_proxy = "export http_proxy=\""+proxystr+";\"\n"
    https_proxy = "export https_proxy=\""+proxystr+";\"\n"
    ftp_proxy = "export ftp_proxy=\""+proxystr+";\"\n"
    bashrc = os.path.join(os.getenv("HOME"), ".bashrc")

    if os.path.isfile(bashrc):
        replacements = 0
        for line in fileinput.input(bashrc, inplace = True):
            if re.match(r'export http_proxy=', line)!= None:
                replacements = replacements + 1
                sys.stdout.write(http_proxy)
            elif re.match(r'export https_proxy=', line)!= None:
                sys.stdout.write(https_proxy)
                replacements = replacements + 1
            elif re.match(r'export ftp_proxy=', line)!= None:
                sys.stdout.write(ftp_proxy)
                replacements = replacements + 1
            else:
                sys.stdout.write(line)
        if replacements == 0:
           f = open(bashrc, 'a')
           f.write(http_proxy)
           f.write(https_proxy)
           f.write(ftp_proxy)
           f.close()
           print("Proxy settings not found, thus appended")
        elif replacements == 3:
           print("Proxy settings successfully changed")
    else:
        f = open(bashrc, 'w')
        f.write(http_proxy)
        f.write(https_proxy)
        f.write(ftp_proxy)
        f.close()
        print("Created new '"+bashrc+"' with new proxy settings")

if __name__ == '__main__':
    main()
